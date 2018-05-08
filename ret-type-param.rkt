#lang racket

(require "syn-tree.rkt")

(provide (contract-out [current-ret-type parameter?]))

(define current-ret-type
  (make-parameter none-t))

