(ns ol.protocol53.http-test
  (:require
   [clojure.java.io :as io]
   [fulcro-spec.core :refer [=> assertions behavior specification]]
   [ol.protocol53.http :as http])
  (:import
   [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
   [java.io ByteArrayInputStream File IOException InputStream OutputStream]
   [java.net InetSocketAddress]
   [java.net.http HttpClient HttpClient$Redirect HttpTimeoutException]
   [java.nio.charset Charset StandardCharsets]
   [java.util Locale]
   [java.util.concurrent CountDownLatch TimeUnit]))

(defn- header-values [values]
  (if (sequential? values) values [values]))

(defn- request-headers [headers]
  (into {}
        (map (fn [[name values]]
               [(.toLowerCase ^String name Locale/ROOT) (vec values)]))
        headers))

(defn- request-view [^HttpExchange exchange]
  (let [uri (.getRequestURI exchange)]
    {:method    (.getRequestMethod exchange)
     :path      (.getPath uri)
     :raw-query (.getRawQuery uri)
     :headers   (request-headers (.getRequestHeaders exchange))
     :body      (slurp (.getRequestBody exchange) :encoding "UTF-8")}))

(defn- send-response!
  [^HttpExchange exchange {:keys [body charset headers status]
                           :or   {body    ""
                                  charset StandardCharsets/UTF_8
                                  headers {}
                                  status  200}}]
  (doseq [[name values] headers
          value         (header-values values)]
    (.add (.getResponseHeaders exchange) (str name) (str value)))
  (if (= status 204)
    (do
      (.sendResponseHeaders exchange status -1)
      (.close exchange))
    (let [bytes (.getBytes ^String body ^Charset charset)]
      (.sendResponseHeaders exchange status (alength bytes))
      (with-open [^OutputStream output (.getResponseBody exchange)]
        (.write output bytes)))))

(defn- recording-handler [calls response-fn]
  (fn [exchange]
    (let [request (request-view exchange)]
      (swap! calls conj request)
      (send-response! exchange (response-fn request)))))

(defn- with-server [handler f]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext
     server
     "/"
     (reify HttpHandler
       (handle [_ exchange]
         (handler exchange))))
    (.start server)
    (try
      (let [port (.getPort (.getAddress server))]
        (f {:base-uri (str "http://127.0.0.1:" port)
            :port     port}))
      (finally
        (.stop server 0)))))

(defn- thrown [f]
  (try
    (f)
    nil
    (catch Throwable cause
      cause)))

(defn- exception-view [cause]
  (when cause
    {:class   (class cause)
     :message (ex-message cause)
     :data    (ex-data cause)}))

(def successful-response
  {:status  200
   :headers {"content-type" ["text/plain"]}
   :body    "ok"
   :version :http1.1})

(specification "The synchronous HTTP client"
  (behavior "normalizes requests before calling a function transport"
    (let [request_         (atom nil)
          default-request_ (atom nil)
          response         successful-response
          result           (http/request
                            {:uri          {:scheme "https"
                                            :host   "api.example.test"
                                            :port   8443
                                            :path   "/v1/items"
                                            :query  "existing=yes"}
                             :method       :patch
                             :headers      {:x-keyword   "keyword"
                                            "X-Repeated" ["one" "two"]}
                             :query-params (array-map :tag ["a b" "x/y"]
                                                      :empty [])
                             :form-params  (array-map :term "café au lait")
                             :timeout      321
                             :version      :http1.1
                             :ignored      :value
                             :client       (fn [request]
                                             (reset! request_ request)
                                             response)})
          default-result   (http/request
                            {:uri    "https://example.test/default"
                             :client (fn [request]
                                       (reset! default-request_ request)
                                       response)})]

      (assertions
        "returns the transport response"
        result => response
        default-result => response
        "passes a normalized request"
        (some-> @request_
                (update :uri str)
                (select-keys [:uri :method :headers :body :timeout :version]))
        => {:uri     (str "https://api.example.test:8443/v1/items"
                          "?existing=yes&tag=a+b&tag=x%2Fy")
            :method  :patch
            :headers {"x-keyword"    "keyword"
                      "X-Repeated"   ["one" "two"]
                      "Content-Type" "application/x-www-form-urlencoded"}
            :body    "term=caf%C3%A9+au+lait"
            :timeout 321
            :version :http1.1}
        "supplies default method and version values"
        (some-> @default-request_ (select-keys [:method :version]))
        => {:method :get :version :http2})))

  (behavior "sends normalized requests and converts real Java responses"
    (let [calls    (atom [])
          response (with-server
                     (recording-handler
                      calls
                      (constantly
                       {:status  207
                        :headers {"Content-Type" "text/plain; charset=ISO-8859-1"
                                  "X-One"        "one"
                                  "X-Multi"      ["first" "second"]}
                        :charset StandardCharsets/ISO_8859_1
                        :body    "café"}))
                     (fn [{:keys [port]}]
                       (http/request
                        {:uri          {:scheme "http"
                                        :host   "127.0.0.1"
                                        :port   port
                                        :path   "/search"
                                        :query  "existing=yes"}
                         :headers      {:x-keyword   "keyword"
                                        "X-Repeated" ["one" "two"]}
                         :query-params (array-map :term "café au lait"
                                                  :tag ["a/b" "c d"]
                                                  :empty [])
                         :ignored      :value})))
          request  (first @calls)]

      (assertions
        "uses the default method and appends encoded query parameters"
        (some-> request (select-keys [:method :path :raw-query :body]))
        => {:method    "GET"
            :path      "/search"
            :raw-query (str "existing=yes&term=caf%C3%A9+au+lait"
                            "&tag=a%2Fb&tag=c+d")
            :body      ""}
        "emits keyword and repeated request headers"
        (some-> request :headers (select-keys ["x-keyword" "x-repeated"]))
        => {"x-keyword"  ["keyword"]
            "x-repeated" ["one" "two"]}
        "returns the status, decoded body, and negotiated version"
        (when (map? response)
          (select-keys response [:status :body :version]))
        => {:status 207 :body "café" :version :http1.1}
        "keeps every response header value in a vector"
        (select-keys (:headers response) ["x-one" "x-multi"])
        => {"x-one"   ["one"]
            "x-multi" ["first" "second"]})))

  (behavior "sends string, file, and input-stream bodies with arbitrary methods"
    (let [calls (atom [])
          file  (File/createTempFile "protocol53-http-" ".txt")]
      (try
        (spit file "file café" :encoding "UTF-8")
        (let [results
              (with-server
                (recording-handler calls (constantly {:status 204}))
                (fn [{:keys [base-uri]}]
                  [(http/request {:uri    (str base-uri "/string")
                                  :method :patch
                                  :body   "string body"})
                   (http/request {:uri    (str base-uri "/file")
                                  :method "PROPFIND"
                                  :body   file})
                   (with-open [input (ByteArrayInputStream.
                                      (.getBytes "stream café"
                                                 StandardCharsets/UTF_8))]
                     (http/request {:uri    (str base-uri "/stream")
                                    :method :post
                                    :body   input}))]))]

          (assertions
            "returns every response"
            (mapv :status results) => [204 204 204]
            "passes each body through its matching Java publisher"
            (mapv #(select-keys % [:method :path :body]) @calls)
            => [{:method "PATCH" :path "/string" :body "string body"}
                {:method "PROPFIND" :path "/file" :body "file café"}
                {:method "POST" :path "/stream" :body "stream café"}]))
        (finally
          (.delete file)))))

  (behavior "form-encodes request bodies without replacing an explicit content type"
    (let [calls   (atom [])
          results (with-server
                    (recording-handler calls (constantly {:status 200}))
                    (fn [{:keys [base-uri]}]
                      [(http/request
                        {:uri         (str base-uri "/automatic")
                         :method      :post
                         :form-params (array-map :space "a b" :slash "a/b")})
                       (http/request
                        {:uri         (str base-uri "/explicit")
                         :method      :post
                         :headers     {"cOnTeNt-TyPe" "application/custom"}
                         :form-params (array-map :space "a b")})]))]

      (assertions
        (mapv :status results) => [200 200]
        (mapv (fn [request]
                {:body         (:body request)
                 :content-type (get-in request [:headers "content-type"])})
              @calls)
        => [{:body         "space=a+b&slash=a%2Fb"
             :content-type ["application/x-www-form-urlencoded"]}
            {:body         "space=a+b"
             :content-type ["application/custom"]}])))

  (behavior "rejects conflicting or unsupported request values"
    (let [base-request {:uri "https://example.test" :client (constantly successful-response)}
          requests     [(assoc base-request :body "body" :form-params {:x "y"})
                        (assoc base-request :body (Object.))
                        (assoc base-request :version :http3)
                        (assoc base-request :as :bytes)
                        (assoc base-request :uri 42)]]

      (assertions
        (mapv #(some-> (thrown (fn [] (http/request %))) class) requests)
        => [IllegalArgumentException
            IllegalArgumentException
            IllegalArgumentException
            IllegalArgumentException
            IllegalArgumentException])))

  (behavior "applies the same status policy to function transports"
    (let [allowed-statuses [200 201 202 203 204 205 206 207
                            300 301 302 303 304 307]
          responses        (mapv (fn [status]
                                   (http/request
                                    {:uri    "https://example.test"
                                     :client (constantly
                                              (assoc successful-response
                                                     :status status))}))
                                 allowed-statuses)
          error-response   {:status  418
                            :headers {"x-error" ["teapot"]}
                            :body    "short and stout"
                            :version :http2}
          cause            (thrown
                            #(http/request
                              {:uri    "https://example.test"
                               :client (constantly error-response)}))
          returned         (http/request
                            {:uri    "https://example.test"
                             :throw  false
                             :client (constantly error-response)})]

      (assertions
        "returns every explicitly allowed status"
        (mapv :status responses) => allowed-statuses
        "throws the complete response for other statuses"
        (exception-view cause)
        => {:class   clojure.lang.ExceptionInfo
            :message "Exceptional status code: 418"
            :data    error-response}
        "returns exceptional statuses when throwing is disabled"
        returned => error-response)))

  (behavior "applies status policy to real Java responses"
    (let [calls         (atom [])
          [cause returned]
          (with-server
            (recording-handler
             calls
             (constantly {:status  418
                          :headers {"X-Error" "teapot"}
                          :body    "short and stout"}))
            (fn [{:keys [base-uri]}]
              [(thrown #(http/request {:uri     (str base-uri "/teapot")
                                       :version :http1.1}))
               (http/request {:uri     (str base-uri "/teapot")
                              :throw   false
                              :version :http1.1})]))
          error-data    (when-let [data (ex-data cause)]
                          (update data :headers select-keys ["x-error"]))
          returned-view (when (map? returned)
                          (update returned :headers select-keys ["x-error"]))
          expected      {:status  418
                         :headers {"x-error" ["teapot"]}
                         :body    "short and stout"
                         :version :http1.1}]

      (assertions
        "throws the converted response by default"
        (select-keys (exception-view cause) [:class :message])
        => {:class   clojure.lang.ExceptionInfo
            :message "Exceptional status code: 418"}
        error-data => expected
        "returns the converted response when throwing is disabled"
        returned-view => expected
        (mapv :path @calls) => ["/teapot" "/teapot"])))

  (behavior "streams response bodies before EOF"
    (let [first-chunk-written (CountDownLatch. 1)
          first-event-read    (CountDownLatch. 1)
          result
          (with-server
            (fn [^HttpExchange exchange]
              (.set (.getResponseHeaders exchange)
                    "Content-Type"
                    "text/event-stream")
              (.sendResponseHeaders exchange 200 0)
              (with-open [^OutputStream output (.getResponseBody exchange)]
                (.write output
                        (.getBytes "data: one\n\n" StandardCharsets/UTF_8))
                (.flush output)
                (.countDown first-chunk-written)
                (when (.await first-event-read 10 TimeUnit/SECONDS)
                  (.write output
                          (.getBytes "data: two\n\n" StandardCharsets/UTF_8))
                  (.flush output))))
            (fn [{:keys [base-uri]}]
              (let [response-future
                    (future
                      (http/request {:uri   (str base-uri "/events")
                                     :as    :stream
                                     :throw false}))]
                (try
                  (when-not (.await first-chunk-written 5 TimeUnit/SECONDS)
                    (throw (IllegalStateException.
                            "SSE server did not write its first chunk")))
                  (let [response (deref response-future 2000 ::pending)]
                    (if (= ::pending response)
                      (do
                        (.countDown first-event-read)
                        (let [late-response (deref response-future 5000 ::pending)
                              body          (when (map? late-response)
                                              (:body late-response))]
                          (when (instance? java.io.Closeable body)
                            (.close ^java.io.Closeable body))
                          (when (= ::pending late-response)
                            (future-cancel response-future)))
                        {:returned-before-eof? false})
                      (with-open [^java.io.BufferedReader reader
                                  (io/reader (:body response)
                                             :encoding "UTF-8")]
                        (let [first-event [(.readLine reader)
                                           (.readLine reader)]]
                          (.countDown first-event-read)
                          {:returned-before-eof? true
                           :status               (:status response)
                           :lines                (into first-event
                                                       [(.readLine reader)
                                                        (.readLine reader)])}))))
                  (finally
                    (.countDown first-event-read)
                    (when-not (future-done? response-future)
                      (future-cancel response-future)))))))]

      (assertions
        result => {:returned-before-eof? true
                   :status               200
                   :lines                ["data: one" "" "data: two" ""]})))

  (behavior "keeps exceptional streamed bodies available in exception data"
    (let [result
          (with-server
            (fn [exchange]
              (send-response! exchange {:status 418 :body "short and stout"}))
            (fn [{:keys [base-uri]}]
              (let [cause    (thrown #(http/request
                                       {:uri (str base-uri "/teapot")
                                        :as  :stream}))
                    response (ex-data cause)]
                (with-open [^InputStream stream (:body response)]
                  {:exception-class (class cause)
                   :message         (ex-message cause)
                   :status          (:status response)
                   :stream?         (instance? InputStream stream)
                   :body            (slurp stream :encoding "UTF-8")}))))]

      (assertions
        result => {:exception-class clojure.lang.ExceptionInfo
                   :message         "Exceptional status code: 418"
                   :status          418
                   :stream?         true
                   :body            "short and stout"})))

  (behavior "times out a request that exceeds its millisecond budget"
    (let [cause
          (with-server
            (fn [exchange]
              (Thread/sleep 250)
              (try
                (send-response! exchange {:body "late"})
                (catch IOException _)))
            (fn [{:keys [base-uri]}]
              (thrown #(http/request {:uri     (str base-uri "/slow")
                                      :timeout 25}))))]

      (assertions
        (some-> cause class) => HttpTimeoutException)))

  (behavior "leaves redirect policy to the Java client"
    (let [calls  (atom [])
          client (-> (HttpClient/newBuilder)
                     (.followRedirects HttpClient$Redirect/ALWAYS)
                     (.build))
          [default-response custom-response]
          (with-server
            (recording-handler
             calls
             (fn [{:keys [path]}]
               (case path
                 "/redirect" {:status  302
                              :headers {"Location" "/final"}
                              :body    "redirect"}
                 "/final"    {:status 200 :body "final"})))
            (fn [{:keys [base-uri]}]
              [(http/request {:uri (str base-uri "/redirect")})
               (http/request {:uri    (str base-uri "/redirect")
                              :client client})]))]

      (assertions
        "does not follow redirects by default"
        (when (map? default-response)
          (select-keys default-response [:status :body]))
        => {:status 302 :body "redirect"}
        "honors a supplied client's redirect policy"
        (when (map? custom-response)
          (select-keys custom-response [:status :body]))
        => {:status 200 :body "final"}
        "only the supplied client reaches the target"
        (mapv :path @calls) => ["/redirect" "/redirect" "/final"]))))
