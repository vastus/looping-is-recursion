(ns looping-is-recursion)

(defn power [base exp]
  (let [p (fn [n k]
            (cond
              (and (pos? n) (zero? k)) 1
              (and (neg? n) (zero? k)) -1
              (== k 1) n
              :else (recur (* n base) (dec k))))]
    (p base exp)))

(defn last-element [a-seq]
  (let [lasti (fn [s]
                (cond
                  (empty? (rest s)) (first s)
                  :else (recur (rest s))))]
    (lasti a-seq)))

(defn seq= [a-seq b-seq]
  (loop [a a-seq
         b b-seq]
    (cond
      (and (empty? a) (empty? b)) true
      (or (empty? a) (empty? b)) false
      (== (first a) (first b)) (recur (rest a) (rest b))
      :else false)))

(defn find-first-index [predicate a-seq]
  (loop [idx 0
         pred predicate
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) idx
      :else (recur (inc idx) pred (rest s)))))

(defn count-avg [sum elems]
  (cond
    (and (zero? sum) (zero? elems)) 0
    :else (/ sum elems)))

(defn avg [a-seq]
  (loop [sum 0
         a a-seq]
    (cond
      (empty? a) (count-avg sum (count a-seq))
      :else (recur (+ sum (first a)) (rest a)))))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn parity [a-seq]
  (loop [a a-seq
         s #{}]
    (cond
      (empty? a) s
      :else (recur (rest a) (toggle s (first a))))))

(defn fast-fibo [no]
  (loop [n no a 0 b 1 fibo 1]
    (cond
      (== no 0) 0
      (< n 2) fibo
      :else (recur (dec n) b (+ a b) (+ a b)))))

(defn cut-at-repetition [a-seq]
  (loop [a a-seq dist []]
    (cond
      (empty? a) dist
      (some #(= % (first a)) dist) dist
      :else (recur (rest a) (conj dist (first a))))))

