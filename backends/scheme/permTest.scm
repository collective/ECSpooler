(define (remove-item item list)
  (if (null? list) 
    '()
    (if (equal? (car list) item)
      (cdr list)
      (cons (car list) (remove-item item (cdr list))) 
    )
  )
)

(define (is-permutation list1 list2)
  (if (null? list1) (null? list2)
    (if (member (car list1) list2)
      (is-permutation (cdr list1) (remove-item (car list1) list2))
      #f
    )
  )
)

(define (test a b) (is-permutation a b) )
