(defn read-file
  "Receives a filename, reads a file and returns a vector of hexa strings."
  [filename]
  (with-open [reader (clojure.java.io/reader filename)]
    (reduce (fn
              [hex-vec line]
              (let [trimmed-string (clojure.string/replace (clojure.string/trim line) #"'" "")]
                (if-not (clojure.string/blank? trimmed-string)
                  (conj hex-vec trimmed-string)
                  (conj hex-vec)))) [] (line-seq reader))))

(defn transform-into-bitstrings
  "Transforms a given hexadecimal string into a string of bits."
  [hexa-vec]
  (reduce (fn
            [bit-vec hexa-string]
            (conj bit-vec (.toString (new java.math.BigInteger hexa-string 16) 2))) [] hexa-vec))

(defn adjust-strings
  "Adjusts all strings within a given vector to have 20k bits."
  [bit-vector]
  (map (fn
         [bit-string]
         (if-not (= (count bit-string) 20000)
           (str "0" bit-string)
           bit-string)) bit-vector))

(defn check-monobit
  "Checks if a given count of ones is within a pre-defined boundary."
  [one-count]
  {:one-count one-count
   :passed? (and (> one-count 9654)
                 (< one-count 10346))})

(defn monobit-test
  "Checks if a given bitstring passes the monobit test. Returns a hash."
  [bit-string]
  (loop [[curr-bit & rest-bits] bit-string one-count 0]
    (cond
      (nil? curr-bit) (check-monobit one-count)
      (= \1 curr-bit) (recur rest-bits (inc one-count))
      :else (recur rest-bits one-count))))

(defn check-poker
  "Does all the logic related to the poker-test."
  [result]
  (let [sum (reduce (fn
                      [value map-t]
                      (+  (* (nth map-t 1) (nth map-t 1)) value)) 0 result)]
    (let [partial (- (* sum 16/5000) 5000)]
      {:result (float partial)
       :nibbles result
       :passed? (and (> partial 1.03 )
                     (< partial 57.4))})))

(defn poker-test
  "Checks if a string is approved by the poker test"
  [bit-string]
  (loop [lower-bound 0 upper-bound 4 results-hash {}]
    (if-not (> upper-bound (count bit-string))
      (let [temp-string (subs bit-string lower-bound upper-bound)]
        (recur (+ lower-bound 4)
               (+ upper-bound 4)           
               (assoc results-hash temp-string (inc (results-hash temp-string 0)))))
      (check-poker results-hash))))

(defn long-run-test
  [bit-string]
  {:passed? (if-not (re-find #"1{34}|0{34}" bit-string)
              true
              false)})

(defn get-regex-count
  "Counts the number of regexp matches within a string."
  [og-string regexp]
  (count (re-seq regexp og-string)))

(defn runs?
  "Returns the quantity of runs within a certain range."
  [og-string regexp lower-bound upper-bound]
  (let [count (get-regex-count og-string regexp)]
    {:count count
     :pass? (and (> count lower-bound)
                 (< count upper-bound))}))

(defn runs-test
  "Performs the run test by applying a series of regexes."
  [bit-string]
  (let [zero-matches {:z-1 (runs? bit-string #"(?<=1)01|10$|^01" 2267 2733)
                      :z-2 (runs? bit-string #"(?<=1)0{2}1|10{2}$|^0{2}1" 1079 1421)
                      :z-3 (runs? bit-string #"(?<=1)0{3}1|10{3}$|^0{3}1" 502 748)
                      :z-4 (runs? bit-string #"(?<=1)0{4}1|10{4}$|^0{4}1" 223 402)
                      :z-5 (runs? bit-string #"(?<=1)0{5}1|10{5}$|^0{5}1" 90 223)
                      :z-6+ (runs? bit-string #"(?<=1)0{6,}1|10{6,}$|^0{6,}1" 90 223)}
        one-matches {:o-1 (runs? bit-string #"(?<=0)10|01$|^10" 2267 2733)
                     :o-2 (runs? bit-string #"(?<=0)1{2}0|01{2}$|^1{2}0" 1079 1421)
                     :o-3 (runs? bit-string #"(?<=0)1{3}0|01{3}$|^1{3}0" 502 748)
                     :o-4 (runs? bit-string #"(?<=0)1{4}0|01{4}$|^1{4}0" 223 402)
                     :o-5 (runs? bit-string #"(?<=0)1{5}0|01{5}$|^1{5}0" 90 223)
                     :o-6+ (runs? bit-string #"(?<=0)1{6,}0|01{6,}$|^1{6,}0" 90 223)}]
    {:zero-seqs zero-matches
     :one-seqs one-matches}))

(defn test-all
  "Tests all strings and exports the final result into /tmp/resultados.txt."
  [bit-vec]
  (spit "/tmp/resultados.txt" "\n")
  (doseq [id (range 1 (inc (count bit-vec)))
          :let [single-bit-string (nth bit-vec (- id 1))]]
    (spit "/tmp/resultados.txt"
          {:id id
           :monobit (monobit-test single-bit-string)
           :long-run (long-run-test single-bit-string)}
          :append true)
    (spit "/tmp/resultados.txt" "\n" :append true)
    (spit "/tmp/resultados.txt" (poker-test single-bit-string) :append true)
    (spit "/tmp/resultados.txt" "\n" :append true)
    (spit "/tmp/resultados.txt" (runs-test single-bit-string) :append true)
    (spit "/tmp/resultados.txt" "\n" :append true)))

(defn test-strings
  []
  (let [hexa-vec (read-file "keys.txt")]
    (let [adjusted-bit-vec (adjust-strings (transform-into-bitstrings hexa-vec))]
      (test-all adjusted-bit-vec))))

(test-strings)
