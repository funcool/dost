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

(ns dost.core.hash
  (:refer-clojure :exclude [reset! -reset])
  (:require [cljs.nodejs :as node]
            [promesa.core :as p :include-macros true]
            [dost.core.stream :as stream]
            [dost.core.buffer :as buffer]
            [dost.core.codecs :as codecs]))

(def ^:private crypto (node/require "crypto"))

;; --- Protocols

(defprotocol IDigest
  (-digest [input engine] "Low level interface, always returns bytes"))

(defprotocol IEngine
  "Hash engine common interface definition."
  (-reset [_] "Reset the hash engine to its initial state.")
  (-update [_ input] "Update bytes in a current instance.")
  (-end [_] "Return the computed mac and reset the engine."))

;; --- Types & Impl

(deftype Digest [id ^:mutable __hash]
  IEngine
  (-reset [it]
    (let [hash (.createHash crypto id)]
      (set! __hash hash)
      it))

  (-update [it data]
    (let [data (buffer/from data)]
      (.update __hash data)
      it))

  (-end [it]
    (.digest __hash)))

;; --- Low Level API

(defn reset!
  [engine]
  (-reset engine))

(defn update!
  [engine input]
  (-update engine input))

(defn end!
  [engine]
  (-end engine))

;; --- Impl High Level API

(defn- hash-plain-data
  [input engine]
  (p/do*
   (-reset engine)
   (-update engine input)
   (-end engine)))

(defn- hash-stream-data
  [input engine]
  (p/promise
   (fn [resolve reject]
     (letfn [(on-data [data]
               (-update engine data))
             (on-end []
               (resolve (-end engine)))
             (on-error [error]
               (reject error))]
       (-reset engine)
       (doto input
         (.on "data" on-data)
         (.on "end" on-end)
         (.on "error" on-error))))))

(defn- resolve-digest
  [engine]
  (cond
    (keyword? engine)
    (Digest. (name engine) nil)

    (string? engine)
    (Digest. engine nil)

    (instance? Digest engine)
    engine

    :else
    (throw (ex-info "Invalid engine parameter" {}))))

(extend-protocol IDigest
  buffer/Buffer
  (-digest [input engine]
    (hash-plain-data input engine))

  string
  (-digest [input engine]
    (hash-plain-data input engine))

  stream/Readable
  (-digest [input engine]
    (hash-stream-data input engine)))

;; --- Public API

(defn digest
  "Generic function for create cryptographic hash."
  [input alg-or-engine]
  (let [engine (resolve-digest alg-or-engine)]
    (-digest input engine)))

(defn sha1
  [input]
  (digest input :sha1))

(defn md5
  [input]
  (digest input :md5))

(defn sha256
  [input]
  (digest input :sha256))

(defn sha384
  [input]
  (digest input :sha256))

(defn sha512
  [input]
  (digest input :sha256))
