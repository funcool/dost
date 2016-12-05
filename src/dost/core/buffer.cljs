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

(ns dost.core.buffer
  "A byte array abstraction on top of nodejs buffer."
  (:refer-clojure :exclude [concat])
  (:require [cljs.nodejs :as node]))

;; --- Constants

(def ^:private buffer (node/require "buffer"))
(def ^:private crypto (node/require "crypto"))
(def Buffer (.-Buffer buffer))

;; --- Public API

(defn buffer?
  [v]
  (.isBuffer Buffer v))

(defn from
  "Coerce to Buffer.

  If an other buffer is provided, it will be returned as is.
  In contrary to the nodejs `Buffer.from` function it does not
  performs the copy of the buffer."
  ([s]
   (if (buffer? s)
     s
     (.from Buffer s "utf8")))
  ([s encoding]
   (if (buffer? s)
     s
     (.from Buffer s (name encoding)))))

(defn alloc
  ([size]
   (.alloc Buffer size 0))
  ([size fill]
   (.alloc Buffer size fill)))

(defn copy
  ([input]
   {:pre [(buffer? input)]}
   (let [target (alloc (count input))]
     (.copy input target)
     target))
  ([input start]
   (copy input start (count input)))
  ([input start end]
   (let [target (alloc (- end start))]
     (.copy input target 0 start end)
     target)))

(defn slice
  "Returns a new Buffer that references the same memory
  as the original, but offset and cropped by the start
  and end indices."
  ([input start]
   {:pre [(buffer? input)]}
   (.slice input start))
  ([input start end]
   {:pre [(buffer? input)]}
   (.slice input start end)))

(defn ^boolean equals?
  "Compare two byte buffer in a constant time."
  [a b]
  (cond
    (or (not (buffer? a))
        (not (buffer? b)))
    false

    (not= (.-length a)
          (.-length b))
    false

    :else
    (loop [i 0 c 0]
      (if (< i (.-length a))
        (recur (inc i)
               (bit-or c (bit-xor (aget a i) (aget b i))))
        (== c 0)))))

(defn concat
  "Given N number of byte arrays, concat them in
  one unique byte array and return it."
  [& parts]
  (.concat Buffer (into-array parts)))

;; --- Implementation

(extend-type js/Buffer
  ICounted
  (-count [it]
    (.-length it))

  IPrintWithWriter
  (-pr-writer [mv writer _]
    (let [size (count mv)
          params {:size size}
          out (->> (partition 2 (.toString mv "hex"))
                   (map #(apply str %))
                   (interpose " ")
                   (apply str))]
      (-write writer (str "#<Buffer " out ">")))))
