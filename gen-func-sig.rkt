#lang racket

(require "syn-tree.rkt"
         "def-table.rkt")

(provide gen-func^)

(define-signature gen-func^
  ((contracted [gen-func (-> symbol?
                             type?
                             (listof syn-param?)
                             syn-prog?
                             def-table/c
                             (values list?
                                     def-table/c))])))

