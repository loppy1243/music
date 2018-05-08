#lang racket

(provide (contract-out [gen-mark (->* ()
                                      ((or/c symbol?
                                             string?))
                                      mark?)])
         (struct-out mark))

(struct mark (val))

(define (gen-mark [?pre #f])
  (mark (gensym (string-append (if ?pre
                                 (cond [(symbol? ?pre)
                                        (symbol->string ?pre)]
                                       [(string? ?pre)
                                        ?pre])
                                 "")
                               "_mark_"))))


