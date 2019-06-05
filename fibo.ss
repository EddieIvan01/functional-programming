;; 1 1 2 3 5 8 13 21

(define (fibo count)
  (define (fibo-iter lst a b _count)
    (if (> _count 0)
        (fibo-iter (append lst (list (+ a b))) b (+ a b) (- _count 1))
        lst))
  (case count
        [1 '(1)]
        [2 '(1 1)]
        [else (fibo-iter '(1 1) 1 1 (- count 2))]))


(define (fib x)
  (if (<= x 2)
      1
      (+ (fib (- x 1)) (fib (- x 2)))))


(display (fibo 20))
