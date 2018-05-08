#lang racket/unit

(require "syn-tree.rkt"
         "def-table.rkt"
         "params-param.rkt"
         "mark.rkt"
         "exp-prog-sig.rkt"
         "gen-func-sig.rkt")

(import exp-prog^)
(export gen-func^)

(define (gen-func id type ?params body def-table)
  (let ([enter-m (gen-mark id)]
        [?ps (and ?params
                  (let ([ret (make-hasheq)])
                    (let loop ([params ?params]
                               [i 1])
                      (if (null? params)
                        ret
                        (begin
                          (hash-set! ret
                                     (syn-id-val (syn-param-id (car params)))
                                     (cons i
                                           (syn-param-type (car params))))
                          (loop (cdr params)
                                (add1 i)))))))])
    (parameterize ([current-params ?ps])
      (values `((eval-ins-mark ,enter-m)
                (eval-push-estk)
                ,@(exp-prog body def-table)
                (eval-pop-estk)
                (eval-ret))
              (def-add def-table
                       id
                       enter-m
                       type
                       (and ?params
                            (map syn-param-type
                                 ?params)))))))

