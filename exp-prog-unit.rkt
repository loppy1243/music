#lang racket/unit

(require "exp-prog-sig.rkt"
         "run-pass-sig.rkt"
         "anon-func-pass-sig.rkt"
         "func-pass-sig.rkt"
         "pass.rkt"
         "def-table.rkt"
         "mark.rkt")

(import run-pass^
        func-pass^
        anon-func-pass^)

(export exp-prog^)

(define (exp-prog prog [def-table (make-def-table)])
  (let*-values ([(m)
                 (gen-mark)]
                [(p1 e1 d1)
                 (do-passes prog def-table
                   func-pass
                   anon-func-pass)]
                [(p2 e2 d2)
                 (do-passes p1 d1
                   run-pass)])
    `((eval-jump ,m)
      ,@e1
      (eval-ins-mark ,m)
      ,@e2)))

