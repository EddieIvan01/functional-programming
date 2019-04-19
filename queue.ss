;; FIFO Queue

(define Queue list)

(define-syntax queue-put!
  (lambda (x)
    (syntax-case x ()
      ((queue-put! queue obj)
      (syntax (set! queue 
                (append queue (list obj))))))))

(define-syntax queue-get!
  (lambda (x)
    (syntax-case x ()
      ((queue-get! queue)
      (syntax (let ([ret (car queue)])
                (set! queue (cdr queue))
                ret))))))

(define (queue-empty? queue)
  (null? queue))


;; TESTING
(define q (Queue))
(display (queue-empty? q)) (newline)
(queue-put! q 1)
(queue-put! q 2)
(display q) (newline)
(display (queue-get! q)) (newline)
(display q)