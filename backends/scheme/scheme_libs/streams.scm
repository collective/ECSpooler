;;; SICP-Stream compatibility definitions
;;;
;;; A google code search for "cons-stream" might help:
;;; <http://www.google.com/codesearch?q=%22cons-stream%22&hl=en&btnG=Search+Code>

;; Special Forms: `delay', `force'
; --> part of R5RS

(module streams mzscheme
  (provide (all-defined))
        
;; Constructor: Special Form `cons-stream'
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

;; Selectors: `head', `tail'
(define head car)
(define (tail stream) (force (cdr stream)))

;; Conatant: `the-empty-stream'
;; Predicate: `empty-stream?'
; old-school, MIT-Scheme-compatible
(define the-empty-stream '()) 
(define empty-stream? null?)
; new-school
;(define the-empty-stream 'the-empty-stream)
;(define (empty-stream? s) (eq? s the-empty-stream))
)