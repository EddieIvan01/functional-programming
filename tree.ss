;; binary tree

(define tree list)

(define (make-tree entry left-branch right-branch)
    (tree left-branch entry right-branch))

(define (get-entry tree)
    (cadr tree))

(define (left-branch tree)
    (car tree))

(define (right-branch tree)
    (caddr tree))

(define (_insert t ele)
    (cond ((null? t) (make-tree ele '() '()))
        ((= ele (get-entry t)) t)
        ((< ele (get-entry t)) 
            (make-tree 
                (get-entry t)
                (_insert (left-branch t) ele)
                (right-branch t)))
        ((> ele (get-entry t)) 
            (make-tree 
                (get-entry t)
                (left-branch t)
                (_insert (right-branch t) ele)))))

(define-syntax tree-insert!
  (lambda (x)
    (syntax-case x ()
      ((tree-insert! tree ele)
      (syntax (set! tree (_insert tree ele)))))))

(define (tree-found? tree ele)
    (cond ((null? tree) #f)
          ((= ele (get-entry tree)) #t)
          ((< ele (get-entry tree))
                (tree-found? (left-branch tree) ele))
          ((> ele (get-entry tree))
                (tree-found? (right-branch tree) ele))))

(define (tree-sum tree)
    (cond ((null? tree) 0)
          ((not (pair? tree)) tree)
          (else (+ (tree-sum (car tree)) (tree-sum (cdr tree))))))


;; TESING
(define t (make-tree 3 '(() 1 (2)) '((4) 5 ())))
(display t)(newline)
(tree-insert! t 7)
(tree-insert! t 8)
(display t)(newline)
(display (tree-found? t 3))(newline)
(display (tree-found? t 9))(newline)
(display (tree-sum t))