#lang racket/signature

(require "pass.rkt"
         "syn-tree.rkt"
         "def-table.rkt")


(contracted [func-pass (-> syn-expr?
                           def-table/c
                           pass-val?)])

