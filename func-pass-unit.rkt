#lang racket/unit

(require "func-pass-sig.rkt"
         "pass.rkt"
         "syn-tree.rkt"
         "gen-func-sig.rkt"
         racket/match)

(import gen-func^)
(export func-pass^)

(define (func-pass x def-table)
  (match x
    [(syn-def (syn-func-id id)
              type
              ?params
              body)
     (let-values ([(e d)
                   (gen-func id type ?params body def-table)])
       (pass-ins/del/def e d))]
    [_
     (pass-cont)]))

