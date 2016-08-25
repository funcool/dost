;; Copyright 2016 Andrey Antukh <niwi@niwi.nz>
;;
;; Licensed under the Apache License, Version 2.0 (the "License")
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns dost.core.codecs
  (:require [cljs.nodejs :as node]
            [dost.core.buffer :as buffer]
            [cuerdas.core :as str]))

(def ^:private buffer? buffer/buffer?)

;; --- Functions

(defn str->bytes
  "Convert a string into bytes buffer."
  ([s]
   (buffer/from s))
  ([s encoding]
   (buffer/from s encoding)))

(defn bytes->str
  [b]
  {:pre [(buffer? b)]}
  (.toString b "utf8"))

(defn bytes->hex
  [b]
  {:pre [(buffer? b)]}
  (.toString b "hex"))

(defn hex->bytes
  [s]
  {:pre [(string? s)]}
  (buffer/from s :hex))

(defn long->bytes
  [^number v]
  (let [buff (buffer/alloc 8)]
    (doto buff
      (.writeUInt32BE (bit-shift-right v 8) 0)
      (.writeUInt32BE (bit-and v 0x00ff) 4))
    buff))

(defn bytes->long
  [b]
  {:pre [(buffer? b)]}
  (let [v1 (.readUInt32BE b 0)
        v2 (.readUInt32BE b 4)]
    (+ (bit-shift-left v1 8) v2)))

(defn b64->str
  ([s] (b64->str s false))
  ([s urlsafe?]
   {:pre [(string? s)]}
   (let [s (if urlsafe?
             (-> (case (mod (count s) 4)
                   2 (str s "==")
                   3 (str s "=")
                   s)
                 (str/replace "-" "+")
                 (str/replace "_" "/"))
             s)
         b (buffer/from s :base64)]
     (.toString b))))

(defn str->b64
  ([s] (str->b64 s false))
  ([s urlsafe?]
   (let [b (buffer/from s)
         r (.toString b "base64")]
     (if urlsafe?
       (-> (str/replace r #"\s" "")
           (str/replace "=" "")
           (str/replace "+" "-")
           (str/replace "/" "_"))
       r))))
