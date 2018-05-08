#lang racket/signature

(require "syn-tree.rkt"
         "def-table.rkt")

(contracted [exp-prog (->* (syn-prog?)
                           (def-table/c)
                           list?)])

