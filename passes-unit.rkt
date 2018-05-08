#lang racket

(require "func-pass-sig.rkt"
         "anon-func-pass-sig.rkt"
         "run-pass-sig.rkt"
         "func-pass-unit.rkt"
         "anon-func-pass-unit.rkt"
         "run-pass-unit.rkt"
         "gen-func-unit.rkt"
         "gen-func-sig.rkt"
         "exp-prog-sig.rkt")

(provide passes@)

(define passes@
  (compound-unit
    (import (ep : exp-prog^))
    (export a b c)
    (link (([gen-f : gen-func^])
           gen-func@
           ep)
      
          (([a : func-pass^])
           func-pass@
           gen-f)
          
          (([b : anon-func-pass^])
           anon-func-pass@
           gen-f)
          
          (([c : run-pass^])
           run-pass@
           ep))))

