(ns ol.protocol53.http
  "Synchronous HTTP transport for bundled protocol53 providers.

  This namespace is an internal provider utility, not a general-purpose HTTP
  client. Loading it requires Java 11 or newer. Advanced client policy remains
  available through a caller-supplied [[java.net.http.HttpClient]]."
  (:require
   [clojure.string :as str])
  (:import
   [java.io File InputStream]
   [java.net URI URLEncoder]
   [java.net.http HttpClient HttpClient$Version HttpRequest HttpRequest$BodyPublishers
    HttpRequest$Builder HttpResponse HttpResponse$BodyHandlers]
   [java.nio.charset StandardCharsets]
   [java.time Duration]
   [java.util Locale]
   [java.util.function Supplier]))

(def ^:private unexceptional-statuses
  #{200 201 202 203 204 205 206 207 300 301 302 303 304 307})

(def ^:private versions
  {:http1.1 HttpClient$Version/HTTP_1_1
   :http2   HttpClient$Version/HTTP_2})

(def ^:private default-client
  (delay (HttpClient/newHttpClient)))

(defn- encoded-value [value]
  (URLEncoder/encode (if (keyword? value) (name value) (str value))
                     StandardCharsets/UTF_8))

(defn- parameter-string [params]
  (str/join "&"
            (map (fn [[key value]]
                   (str (encoded-value key) "=" (encoded-value value)))
                 params)))

(defn- query-string [params]
  (str/join "&"
            (for [[key values] params
                  value        (if (sequential? values) values [values])]
              (str (encoded-value key) "=" (encoded-value value)))))

(defn- request-uri [value]
  (cond
    (string? value)
    (URI/create value)

    (map? value)
    (let [{:keys [scheme host path port query]} value]
      (when-not (and scheme host)
        (throw (IllegalArgumentException.
                "URI component maps require :scheme and :host")))
      (URI. scheme nil host (int (or port -1)) (or path "") query nil))

    :else
    (throw (IllegalArgumentException.
            "HTTP request :uri must be a string or component map"))))

(defn- with-query [^URI uri query]
  (if (empty? query)
    uri
    (let [uri-string       (str uri)
          fragment-index   (.indexOf ^String uri-string "#")
          fragment         (when-not (neg? fragment-index)
                             (subs uri-string fragment-index))
          without-fragment (if fragment
                             (subs uri-string 0 fragment-index)
                             uri-string)
          query-index      (.indexOf ^String without-fragment "?")
          existing-query   (when-not (neg? query-index)
                             (subs without-fragment (inc query-index)))
          base             (if (neg? query-index)
                             without-fragment
                             (subs without-fragment 0 query-index))]
      (URI/create
       (str base
            "?"
            (when (seq existing-query) (str existing-query "&"))
            query
            fragment)))))

(defn- header-name [header]
  (if (keyword? header) (name header) header))

(defn- normalized-headers [headers form?]
  (let [headers (into {}
                      (map (fn [[name values]]
                             [(header-name name) values]))
                      headers)]
    (if (and form?
             (not-any? (fn [[name _]]
                         (= "content-type"
                            (.toLowerCase ^String name Locale/ROOT)))
                       headers))
      (assoc headers "Content-Type" "application/x-www-form-urlencoded")
      headers)))

(defn- validated-version [version]
  (if (contains? versions version)
    version
    (throw (IllegalArgumentException.
            (str "Unsupported HTTP version: " version)))))

(defn- validated-body [body]
  (if (or (nil? body)
          (string? body)
          (instance? File body)
          (instance? InputStream body))
    body
    (throw (IllegalArgumentException.
            (str "Unsupported HTTP request body: " (class body))))))

(defn- normalized-request [request]
  (when (and (contains? request :body)
             (contains? request :form-params))
    (throw (IllegalArgumentException.
            "HTTP request cannot contain both :body and :form-params")))
  (when-not (contains? #{nil :string :stream} (:as request))
    (throw (IllegalArgumentException.
            (str "Unsupported HTTP response representation: " (:as request)))))
  (let [form?   (contains? request :form-params)
        body    (validated-body
                 (if form?
                   (parameter-string (:form-params request))
                   (:body request)))
        query   (query-string (:query-params request))
        uri     (with-query (request-uri (:uri request)) query)
        method  (get request :method :get)
        version (validated-version (get request :version :http2))
        headers (normalized-headers (:headers request) form?)]
    (assoc request
           :uri uri
           :method method
           :headers headers
           :body body
           :version version)))

(defn- body-publisher [body]
  (cond
    (nil? body)
    (HttpRequest$BodyPublishers/noBody)

    (string? body)
    (HttpRequest$BodyPublishers/ofString body)

    (instance? File body)
    (HttpRequest$BodyPublishers/ofFile (.toPath ^File body))

    :else
    (HttpRequest$BodyPublishers/ofInputStream
     (reify Supplier
       (get [_] body)))))

(defn- method-name [method]
  (if (keyword? method)
    (.toUpperCase ^String (name method) Locale/ROOT)
    method))

(defn- java-request [request]
  (let [^HttpRequest$Builder builder (HttpRequest/newBuilder ^URI (:uri request))]
    (.version builder (versions (:version request)))
    (when-let [timeout (:timeout request)]
      (.timeout builder (Duration/ofMillis timeout)))
    (doseq [[name values] (:headers request)
            value         (if (sequential? values) values [values])]
      (.header builder name value))
    (.method builder
             (method-name (:method request))
             (body-publisher (:body request)))
    (.build builder)))

(defn- response-version [version]
  (if (= HttpClient$Version/HTTP_1_1 version) :http1.1 :http2))

(defn- response-map [^HttpResponse response]
  {:status  (.statusCode response)
   :headers (into {}
                  (map (fn [[name values]] [name (vec values)]))
                  (.map (.headers response)))
   :body    (.body response)
   :version (response-version (.version response))})

(defn- send-request [^HttpClient client request]
  (response-map
   (.send client
          ^HttpRequest (java-request request)
          (if (= :stream (:as request))
            (HttpResponse$BodyHandlers/ofInputStream)
            (HttpResponse$BodyHandlers/ofString)))))

(defn- checked-response [response throw?]
  (if (or (= false throw?)
          (contains? unexceptional-statuses (:status response)))
    response
    (throw (ex-info (str "Exceptional status code: " (:status response))
                    response))))

(defn request
  "Sends one synchronous HTTP request and returns its response.

  Options:

  | key             | description
  | ----------------|------------
  | `:uri`          | Required URI string or component map.
  | `:method`       | HTTP method keyword or string (default `:get`).
  | `:headers`      | Map of header names to string or sequential values.
  | `:query-params` | Map of UTF-8 form-encoded query parameters.
  | `:form-params`  | Map encoded as the request body.
  | `:body`         | String, [[java.io.File]], or [[java.io.InputStream]].
  | `:as`           | `:string` (default) or `:stream`.
  | `:timeout`      | Request timeout in milliseconds.
  | `:throw`        | Throw for exceptional statuses unless `false`.
  | `:version`      | `:http1.1` or `:http2` (default `:http2`).
  | `:client`       | [[java.net.http.HttpClient]] or request function.

  Returns `:status`, vector-valued `:headers`, `:body`, and the negotiated
  `:version`. The body is a string by default. With `:as :stream`, it is a
  [[java.io.InputStream]] available after the response headers; the caller must
  exhaust or close it. Streaming callers should normally use `:throw false` to
  retain ownership of the stream for every status. Exceptional statuses throw
  `ExceptionInfo` with the complete response as `ex-data`."
  [request]
  (let [request  (normalized-request request)
        client   (:client request)
        response (if (fn? client)
                   (client request)
                   (send-request (or client @default-client) request))]
    (checked-response response (:throw request))))
