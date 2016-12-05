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

(ns dost.core.nonce
  "Namespace dedicated to provide an abstraction
  for generate a valid secure nonce and random ivs."
  (:require [cljs.nodejs :as node]
            [dost.core.buffer :as buffer]))

(def ^:private crypto (node/require "crypto"))

;; --- Public API

(defn random-bytes
  "Generate a byte array of scpecified length with random
  bytes taken from secure random number generator.
  This method should be used for generate a random
  iv/salt or arbitrary length."
  [length]
  {:pre [(number? length)]}
  (.randomBytes crypto length))

(defn random-nonce
  "Generate a secure nonce based on current time
  and additional random data obtained from secure random
  generator. The minimum value is 8 bytes, and recommended
  minimum value is 32."
  [length]
  {:pre [(number? length) (>= length 8)]}
  (let [buff (.randomBytes crypto length)
        time (.toString (.now js/Date) 16)
        napp (mod 16 (count time))
        more (apply str (repeat napp "0"))
        time (str more time)]
    (.write buff time 0 8 "hex")
   buff))
