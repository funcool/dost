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

(ns dost.core.hmac
  (:refer-clojure :exclude [reset! -reset])
  (:require [cljs.nodejs :as node]
            [promesa.core :as p :include-macros true]
            [dost.core.hash :as hash]
            [dost.core.stream :as stream]
            [dost.core.buffer :as buffer]
            [dost.core.codecs :as codecs]))

(def ^:private crypto (node/require "crypto"))

;; --- Protocols

;; The HMac implementation uses the same protocol declared
;; in the hash namespace because they are completly analogous.
;; The main difference is the final type and its implementation.

;; --- Types & Impl

(deftype HMac [id key ^:mutable __hmac]
  hash/IEngine
  (-reset [it]
    (let [hmac (.createHmac crypto id key)]
      (set! __hmac hmac)
      it))

  (-update [it data]
    (let [data (buffer/from data)]
      (.update __hmac data)
      it))

  (-end [it]
    (.digest __hmac)))

;; --- Low Level API

(defn reset!
  [engine]
  (hash/-reset engine))

(defn update!
  [engine input]
  (hash/-update engine input))

(defn end!
  [engine]
  (hash/-end engine))

;; --- Impl High Level API

(defn- hash-plain-data
  [input engine]
  (p/do*
   (hash/-reset engine)
   (hash/-update engine input)
   (hash/-end engine)))

(defn- hash-stream-data
  [input engine]
  (p/promise
   (fn [resolve reject]
     (letfn [(on-data [data]
               (hash/-update engine data))
             (on-end []
               (resolve (hash/-end engine)))
             (on-error [error]
               (reject error))]
       (hash/-reset engine)
       (doto input
         (.on "data" on-data)
         (.on "end" on-end)
         (.on "error" on-error))))))

(defn- resolve-hmac
  [engine key]
  (let [key (buffer/from key)]
    (cond
      (keyword? engine)
      (HMac. (name engine) key nil)

      (string? engine)
      (HMac. engine key nil)

      (instance? HMac engine)
      engine

      :else
      (throw (ex-info "Invalid engine parameter" {})))))

(extend-protocol hash/IDigest
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
  [input key alg-or-engine]
  (let [engine (resolve-hmac alg-or-engine key)]
    (hash/-digest input engine)))

(defn verify
  [input signature key alg-or-engine]
  (let [engine (resolve-hmac alg-or-engine key)
        input  (buffer/from input)
        signatue (buffer/from signature)
        result (hash/-digest input engine)]
    (buffer/equals? signatue result)))


