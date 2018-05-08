#lang racket

(provide (contract-out [current-params parameter?]))

(define current-params
  (make-parameter (hasheq)
                  (λ (?x)
                    (if ?x
                      ?x
                      (hasheq)))))

