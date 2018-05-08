#lang racket

(require "mark.rkt"
         "syn-tree.rkt"
         "func-exn.rkt"
         anaphoric)

(provide (contract-out [make-def-table (-> empty-hash?)]
                       [def-add (case-> (-> def-table/c
                                            symbol?
                                            def-table-entry?
                                            def-table/c)
                                        (-> def-table/c
                                            symbol?
                                            mark?
                                            type?
                                            (or/c (listof type?)
                                                  #f)
                                            def-table/c))]
                       [def-entry (-> def-table/c
                                      symbol?
                                      def-table-entry?)]
                       [def-mark (-> def-table/c
                                     symbol?
                                     mark?)]
                       [def-type (-> def-table/c
                                     symbol?
                                     type?
                                     )]
                       [def-params (-> def-table/c
                                       symbol?
                                       (or/c (listof type?)
                                             #f))])
         (struct-out def-table-entry)
         def-table/c)

(define (empty-hash? x)
  (and (hash? x)
       (zero? (hash-count x))))

(struct def-table-entry (mark type ?params))

(define def-table/c
  (hash/c symbol?  def-table-entry?))

(define (make-def-table)
  (make-immutable-hasheq))

(define def-add
  (case-lambda
   [(def-table id entry)
    (if (hash-ref def-table id #t)
      (hash-set def-table id entry)
      (raise (make-func-exn:defined id)))]
   [(def-table id mark type ?params)
    (def-add def-table id (def-table-entry mark type ?params))]))

(define (def-entry def-table id)
  (if-let [it (hash-ref def-table id #f)]
    it
    (raise (make-func-exn:undefined id))))

(define (def-mark def-table id)
  (def-table-entry-mark (def-entry def-table id)))

(define (def-type def-table id)
  (def-table-entry-type (def-entry def-table id)))

(define (def-params def-table id)
  (def-table-entry-?params (def-entry def-table id)))

