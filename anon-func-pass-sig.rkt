#lang racket/signature

(require "syn-tree.rkt"
         "pass.rkt"
         "def-table.rkt")

(contracted [anon-func-pass (-> syn-expr?
                                def-table/c
                                pass-val?)])

