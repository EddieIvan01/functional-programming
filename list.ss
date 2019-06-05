;; basic list

(define BList list)

(define (lst-get-first lst)
    (car lst))

(define (lst-get-last lst)
    (if (not (null? (cdr lst)))
        (lst-get-last (cdr lst))
        (car lst)))

(define (lst-len lst)
    (let ([length 0])
        (define (iter l)
            (if (not (null? (cdr l)))
                (begin
                    (set! length (+ length 1))
                        (iter (cdr l)))
                    (+ length 1)))
        (iter lst)))


(define (re l)
  (define (re-iter i t)
    (if (null? i)
        t
        (re-iter (cdr i) (cons (car i) t))))
  (re-iter l '()))


(define (deep-reverse l)
    (define (count-leaves iter)
        (cond ((null? iter) 0)
            ((not (pair? iter)) 1)
            (else (+ (count-leaves (car iter))
                    (count-leaves (cdr iter))))))
    (define (pair-contains-pair? iter)
        (not (= (length iter) (count-leaves iter))))
    (define (rev iter)
        (if (pair? iter)
            (if (pair-contains-pair? iter)
                (cons (rev (car iter)) (rev (cdr iter)))
                (reverse iter))
            iter))
    (rev (reverse l)))


;; TESTING
(define lst (BList 1 2 3 4 5 6 7 8))
(display (lst-get-first lst))(newline)
(display (lst-get-last lst))(newline)
(display (lst-len lst))(newline)


;; TESTING
(define x '((1 2 3) (4 5) 6 7 (8)))
(display x)
(newline)
(display (re x))
(newline)
(display (deep-reverse x))