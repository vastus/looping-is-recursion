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

(defn seq= [seq1 seq2]
  (let [f (fn [a b]
            (cond
              (and (empty? a) (empty? b)) true
              (or (empty? a) (empty? b)) false
              (== (first a) (first b)) (recur (rest a) (rest b))
              :else false))]
    (f seq1 seq2)))

;; TODO: Unable to resolve symbol: ...
;; (defn seq= [a-seq] [b-seq]
;;   (loop [a a-seq
;;          b b-seq]
;;     (cond
;;       (and (empty? a) (empty? b)) true
;;       (or (empty? a) (empty? b)) false
;;       (== (first a) (first b)) (recur (rest a) (rest b))
;;       :else false)))

;; With let
;; (defn find-first-index [pred a-seq]
;;   (let [f (fn [idx pred a-seq]
;;             (cond
;;               (empty? a-seq) nil
;;               (pred (first a-seq)) idx
;;               :else (recur (inc idx) pred (rest a-seq))))]
;;     (f 0 pred a-seq)))

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

;; def fibo(n):
;;     if n == 0: return 0
;;     eka = 0
;;     toka = 1
;;     fibo = 1
;;     for i in range(n-1):
;;         fibo = eka + toka
;;         eka = toka
;;         toka = fibo
;;     return fibo

(defn fast-fibo [no]
  (loop [n no
         eka 0
         toka 1
         fibo 1]
    (cond
      (== no 0) 0
      (== n 0) fibo)))

(defn cut-at-repetition [a-seq]
  [":("])

