#lang racket/signature

(require "syn-tree.rkt"
         "pass.rkt"
         "def-table.rkt")

(contracted [run-pass (-> syn-expr?
                          def-table/c
                          pass-val?)])

