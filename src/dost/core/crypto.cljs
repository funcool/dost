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

(ns dost.core.crypto
  (:refer-clojure :exclude [reset! -reset])
  (:require [cljs.nodejs :as node]
            [promesa.core :as p :include-macros true]
            [dost.core.hash :as hash]
            [dost.core.stream :as stream]
            [dost.core.buffer :as buffer]
            [dost.core.codecs :as codecs]))

(def ^:private crypto (node/require "crypto"))
(def ^:private buffer? buffer/buffer?)

(defprotocol ICipher
  "Hash engine common interface definition."
  (-init [_ opts] "Initialize cipher.")
  (-authtag [_] "Return the authentication tag (only on AEAD ciphers).")
  (-update [_ input] "Update bytes in a current instance.")
  (-end [_] "Return the computed mac and reset the engine."))

(deftype Cipher [id ^:mutable __engine]
  ICipher
  (-init [_ opts]
    (assert (buffer? (:iv opts)) "The `iv` parameter is mandatory.")
    (assert (buffer? (:key opts)) "The `key` parameter is mandatory.")
    (let [{:keys [op key iv padding? tag aad]
           :or {op :encrypt padding? true}} opts
          engine (case op
                   :encrypt (.createCipheriv crypto id key iv)
                   :decrypt (.createDecipheriv crypto id key iv)
                   (throw (ex-info "Invalid operation" {:op op})))]
      (when-not padding? (.setAutoPadding engine false))
      (when (buffer? tag) (.setAuthTag engine))
      (when (buffer? aad) (.setAAD engine aad))
      (set! __engine engine)))

  (-authtag [_]
    (assert __engine "Cipher not initialized.")
    (.getAuthTag __engine))

  (-update [_ input]
    (assert __engine "Cipher not initialized.")
    (.update __engine input))

  (-end [_]
    (assert __engine "Cipher not initialized.")
    (.final __engine)))

;; --- Public API

(def available-ciphers
  (into #{} (map keyword (.getCiphers crypto))))

(defn cipher?
  [v]
  (instance? Cipher v))

(defn cipher
  [alg]
  (assert (available-ciphers alg) "Not supported cipher.")
  (Cipher. (name alg) nil))

(defn init!
  [c opts]
  {:pre [(cipher? c)]}
  (-init c opts))

(defn update!
  [c input]
  {:pre [(cipher? c)]}
  (let [input (buffer/from input)]
    (-update c input)))

(defn end!
  [c]
  (-end c))
