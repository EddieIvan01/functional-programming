;; Test multiple bracket's match

(load "./stack.ss")

(define (left-bracket? s)
  (or (char=? s #\() (char=? s #\{) (char=? s #\[)))

(define (right-bracket? s)
  (or (char=? s #\)) (char=? s #\}) (char=? s #\])))

(define (bracket-pair s)
  (case s
    [#\) #\(]
    [#\] #\[]
    [#\} #\{]))

(define (match? exp)
  (define exp-lst (string->list exp))
  (define s (Stack))
  (define (handle e)
    (cond [(left-bracket? e) (stack-push! s e)]
          [(right-bracket? e) (cond 
                                [(stack-empty? s) (begin (display "not match") (exit))]
                                [(char=? (stack-pop! s) (bracket-pair e)) #t]
                                [else (begin (display "not match") (exit))])]))
  (define (iter exp-s)
    (cond [(null? exp-s) (if (stack-empty? s)
                            (display "match ok")
                            (display "not match"))]
          [else (begin (handle (car exp-s)) (iter (cdr exp-s)))]))
  (iter exp-lst))

(define exp (cadr (command-line)))
(match? exp)