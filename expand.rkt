#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------------------------------------------------------------------------------
;;| The following symbols are returned from this file:
;;|
;;|   eval-call
;;|   eval-ret
;;|   eval-push-estk
;;|   eval-pop-estk
;;|   eval-push-astk
;;|   eval-pop-astk
;;|   eval-ins-mark 
;;|   eval-mov-rval->reg 
;;|   eval-mov-areg->reg 
;;|   eval-mov-int->reg 
;;|   eval-jump 
;;|   eval-swap-reg/areg 
;;|   eval-mov-int->rval 
;;|   eval-note/3
;;|   eval-cmd/3
;;|   eval-push-cstk
;;|   eval-pop-cstk
;;|   eval-mov-reg->recnt 
;;|   eval-dec-recnt
;;|   eval-jump-recnt=int 
;;|   eval-cmd-val/1
;;|   eval-jump-reg!=reg 
;;|   eval-jump-reg=reg  
;;|   eval-jump-reg<=reg 
;;|   eval-jump-reg>=reg 
;;|   eval-jump-reg<reg  
;;|   eval-jump-reg>reg  
;;|
;;| General purpose registers used:
;;|
;;|   reg  -- 3
;;|   areg -- 3
;;|   
;;--------------------------------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "parse.rkt"
         "lex.rkt"
         "passes-unit.rkt"
         "func-pass-sig.rkt"
         "anon-func-pass-sig.rkt"
         "run-pass-sig.rkt"
         "exp-prog-unit.rkt"
         "exp-prog-sig.rkt")

(provide exp-prog)

(define (main)
  (with-input-from-file "test.music"
    (thunk
      (pretty-display (exp-prog (parse (make-lexer lex
                                                   (current-input-port))))))))

(define all@
  (compound-unit
    (import)
    (export es)
    (link (([f : func-pass^]
            [a : anon-func-pass^]
            [r : run-pass^])
           passes@
           es)

          (([es : exp-prog^])
           exp-prog@
           f
           a
           r))))

(define-values/invoke-unit all@
  (import)
  (export exp-prog^))

