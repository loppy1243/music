#!/usr/bin/racket
#lang racket

(require "backend/assembly/assembly.rkt"
         "backend/interpreter.rkt"
         "backend/music-assembly.rkt"
         "backend-sig.rkt"
         "lex.rkt"         
         "parse.rkt"       
         "expand.rkt")

(define output-port (make-parameter (current-output-port)))
(define input-port (make-parameter (current-input-port)))
(define backend (make-parameter "x86_64"))

(displayln (current-command-line-arguments))

(input-port (command-line
              #:once-each
              ["-o" file
                    "Output file."
                    (output-port (open-output-file file
                                                   #:exists 'replace))]
              ["--backend" b
                           "Backend to use."
                           (backend b)]
              
              #:args
              ([file (current-input-port)])
              (open-input-file file)))

(define impl
  (case (backend)
    [("asm")
     music-assembly@]
    [("x86_64")
     assembly@]
    [("interpreter")
     interpreter@]))

(define-values/invoke-unit impl
  (import)
  (export backend^))

(define-namespace-anchor this)

(define (eval-this x)
  (eval x (namespace-anchor->namespace this)))

(define (run port-in port-out)
  (eval-prog (exp-prog (parse (make-lexer lex port-in)))
             eval-this
             port-out))

(run (input-port)
     (output-port))

(close-input-port (input-port))
(close-output-port (output-port))
