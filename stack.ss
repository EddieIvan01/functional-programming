;; FILO Stack

(define Stack list)

(define (stack-top stack)
  (if (null? (cdr stack))
    (car stack)
    (stack-top (cdr stack))))

(define (stack-empty? stack)
  (null? stack))

(define-syntax stack-push!
  (lambda (x)
    (syntax-case x ()
      ((stack-push! stack obj)
      (syntax (set! stack 
          (append stack (list obj))))))))

(define-syntax stack-pop!
  (lambda (x)
    (syntax-case x ()
      ((stack-pop! stack)
      (syntax (let ([top (stack-top stack)])
                (define (remove-last lst ret)
                  (if (null? (cdr lst))
                    ret
                    (remove-last 
                      (cdr lst) (append ret (list (car lst))))))
                (set! stack (remove-last stack '()))
                top))))))


;; TESING
(define s (Stack))
(display (stack-empty? s)) (newline)
(stack-push! s 1)
(stack-push! s 2)
(stack-push! s 3)
(display s) (newline)
(display (stack-empty? s)) (newline)
(display (stack-top s)) (newline)
(display (stack-pop! s)) (newline)
(display s)