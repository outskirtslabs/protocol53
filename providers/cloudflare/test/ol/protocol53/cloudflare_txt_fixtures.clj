(ns ol.protocol53.cloudflare-txt-fixtures
  "Cloudflare TXT presentation examples used by provider tests.")

(def ^:private a254 (apply str (repeat 254 "a")))
(def ^:private a255 (str a254 "a"))

(def encoding-cases
  [{:label   "empty data"
    :data    ""
    :content "\"\""}
   {:label   "ASCII data"
    :data    "v=spf1 -all"
    :content "\"v=spf1 -all\""}
   {:label   "double quotes"
    :data    "a\"b"
    :content "\"a\\\"b\""}
   {:label   "backslashes"
    :data    "a\\b"
    :content "\"a\\\\b\""}
   {:label   "tabs"
    :data    "x\ty"
    :content "\"x\\009y\""}
   {:label   "NUL bytes"
    :data    "x\u0000y"
    :content "\"x\\000y\""}
   {:label   "UTF-8"
    :data    "é"
    :content "\"\\195\\169\""}
   {:label   "exactly 255 octets"
    :data    a255
    :content (str "\"" a255 "\"")}
   {:label   "256 octets"
    :data    (str a255 "a")
    :content (str "\"" a255 "\" \"a\"")}
   {:label   "a multibyte character crossing the chunk boundary"
    :data    (str a254 "é")
    :content (str "\"" a254 "\\195\" \"\\169\"")}
   {:label   "escaped presentation wider than its raw octets"
    :data    (apply str (repeat 256 "\""))
    :content (str "\""
                  (apply str (repeat 255 "\\\""))
                  "\" \"\\\"\"")}])

(def decoding-cases
  [{:label   "quoted ASCII"
    :content "\"hello\""
    :data    "hello"}
   {:label   "escaped double quotes"
    :content "\"a\\\"b\""
    :data    "a\"b"}
   {:label   "escaped backslashes"
    :content "\"a\\\\b\""
    :data    "a\\b"}
   {:label   "decimal escapes"
    :content "\"x\\065y\""
    :data    "xAy"}
   {:label   "escaped tabs"
    :content "\"x\\009y\""
    :data    "x\ty"}
   {:label   "escaped UTF-8"
    :content "\"h\\195\\169llo\""
    :data    "héllo"}
   {:label   "multiple character strings"
    :content "\"a\" \"b\""
    :data    "ab"}
   {:label   "an empty character string"
    :content "\"\""
    :data    ""}
   {:label   "unquoted content"
    :content "hello world"
    :data    "hello world"}
   {:label   "unquoted escapes"
    :content "x\\065y"
    :data    "x\\065y"}
   {:label   "escaped non-digits"
    :content "\"a\\zb\""
    :data    "azb"}])

(def malformed-contents
  [{:label   "unterminated content"
    :content "\"abc"}
   {:label   "a trailing backslash"
    :content "\"abc\\"}
   {:label   "a short decimal escape"
    :content "\"a\\99\""}
   {:label   "data after a character string"
    :content "\"a\"x"}
   {:label   "an out-of-range decimal escape"
    :content "\"\\256\""}
   {:label   "an incomplete decimal escape"
    :content "\"\\09"}])

(def round-trip-values
  [{:label "a long DKIM value"
    :data  (str "v=DKIM1; k=rsa; p="
                (apply str
                       (repeat 9 "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA"))
                "IDAQAB")}
   {:label "mixed Unicode"
    :data  "café ☕ 日本語"}
   {:label "NUL, quotes, and backslashes"
    :data  "\u0000 has \"quotes\" and \\backslashes"}
   {:label "one thousand octets"
    :data  (apply str (repeat 1000 "A"))}])
