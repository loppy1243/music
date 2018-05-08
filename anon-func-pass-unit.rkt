#lang racket/unit

(require "syn-tree.rkt"
         "gen-func-sig.rkt"
         "pass.rkt"
         "ret-type-param.rkt"
         "anon-func-pass-sig.rkt"
         "expand-exn.rkt"
         racket/match)

(import gen-func^)
(export anon-func-pass^)

(define (gen-func/short mark type ?meta def-table)
  (match ?meta
    [(syn-meta prog)
     (gen-func mark
               type
               #f
               prog
               def-table)]
    [_
     (values #f def-table)]))

(define (anon-func-pass x def-table)
  (match x
    [(syn-meta prog)
     (let*-values ([(m)
                    (gensym)]
                   [(e d)
                    (gen-func m
                              none-t
                              #f
                              prog
                              def-table)])
       (pass-ret/def (list (syn-func-call (syn-func-id m)
                                          #f))
                     e
                     d))]
    [(syn-ret (syn-meta prog))
     (let*-values ([(fm)
                    (gensym)]
                   [(e d)
                    (gen-func fm
                              (current-ret-type)
                              #f
                              prog
                              def-table)])
       (pass-ret/def (list (syn-ret (syn-func-call (syn-func-id fm)
                                                   #f)))
                     e
                     d))]
    [(syn-note-c a b)
     (let*-values ([(m1)
                    (gensym)]
                   [(m2)
                    (gensym)]
                   [(e1 d1)
                    (gen-func/short m1
                                    note-base-t
                                    a
                                    def-table)]
                   [(e2 d2)
                    (gen-func/short m2
                                    len-t
                                    b
                                    d1)])
       (pass-ret/def (list (syn-note-c (if e1
                                         (syn-func-call (syn-func-id m1))
                                         a)
                                       (if e2
                                         (syn-func-call (syn-func-id m2))
                                         b)))
                     `(,@(or e1
                             '())
                       ,@(or e2
                             '()))
                     d2))]
    [(syn-if-stmt ifs)
     (anon-func-pass/ifs ifs def-table)]
    [(syn-cmd a b rel)
     (let*-values ([(m1)
                    (gensym)]
                   [(m2)
                    (gensym)]
                   [(e1 d1)
                    (gen-func/short m1
                                    command-t
                                    a
                                    def-table)]
                   [(e2 d2)
                    (gen-func/short m2
                                    cmd-val-t
                                    b
                                    d1)])
       (pass-ret/def (list (syn-cmd (if e1
                                      (syn-func-call (syn-func-id m1)
                                                     #f)
                                      a)
                                    (if e2
                                      (syn-func-call (syn-func-id m2)
                                                     #f)
                                      b)
                                    rel))
                     `(,@(or e1
                             '())
                       ,@(or e2
                             '()))
                     d2))]
    [(syn-func-call id (? list? args))
     (anon-func-pass/func-call id args def-table)]
    [(syn-repeat (syn-meta prog) p)
     (let*-values ([(m)
                    (gensym)]
                   [(e d)
                    (gen-func m
                              num-t
                              #f
                              prog
                              def-table)])
       (pass-ret/def (list (syn-repeat (syn-func-call (syn-func-id m)
                                                      #f)
                                       p))
                     e
                     d))]
    [_
     (pass-cont)]))

(define (anon-func-pass/ifs ifs def-table)
  (let loop ([ifs ifs]
             [ret-s (list)]
             [ret-e (list)]
             [ret-d def-table])
    (if (null? ifs)
      (pass-ret/def (list (syn-if-stmt ret-s))
                    ret-e
                    ret-d)
      (match (car ifs)
        [(syn-if test body)
         (let-values ([(test-s test-e test-d)
                       (anon-func-pass/test test ret-d)])
           (loop (cdr ifs)
                 (append ret-s
                         (list (syn-if test-s body)))
                 (append ret-e
                         test-e)
                 test-d))]
        [(syn-else body)
         (loop (cdr ifs)
               (append ret-s
                       (list (car ifs)))
               ret-e
               ret-d)]
        [_
         (!raise-impossible "Only syn-if, syn-else, can be in syn-if-stmt list\nFrom: anon-func-pass/ifs")]))))

(define (anon-func-pass/test test def-table)
  (match test
    [(syn-test (syn-meta p1) op (syn-meta p2))
     (let*-values ([(m1)
                    (gensym)]
                   [(m2)
                    (gensym)]
                   [(e1 d1)
                    (gen-func m1
                              command-t
                              #f
                              p1
                              def-table)]
                   [(e2 d2)
                    (gen-func m2
                              command-t
                              #f
                              p2
                              d1)])
       (values (syn-test (syn-func-call m1
                                        #f)
                         op
                         (syn-func-call m2
                                        #f))
               (append e1
                       e2)
               d2))]
    [(syn-test (? syn-test? t1) op (? syn-test? t2))
     (let*-values ([(t1-s t1-e t1-d)
                    (anon-func-pass/test t1 def-table)]
                   [(t2-s t2-e t2-d)
                    (anon-func-pass/test t2 t1-d)])
       (values (syn-test t1-s op t2-s)
               (append t1-e
                       t2-e)
               t2-d))]
    [(syn-test (syn-meta p1) op (and (not (? syn-meta?))
                                     b))
     (let*-values ([(m1)
                    (gensym)]
                   [(e d)
                    (gen-func m1
                              command-t
                              #f
                              p1
                              def-table)])
       (values (syn-test (syn-func-call m1
                                        #f)
                         op
                         b)
               e
               d))]
    [(syn-test a op (syn-meta p2))
     (let*-values ([(m2)
                    (gensym)]
                   [(e d)
                    (gen-func m2
                              cmd-val-t
                              #f
                              p2
                              def-table)])
       (values (syn-test a
                         op
                         (syn-func-call m2
                                        #f))
               e
               d))]
    [_
     (values test
             '()
             def-table)]))

(define (anon-func-pass/func-call id args def-table)
  (let loop ([args args]
             [ret-s (list)]
             [ret-e (list)]
             [ret-d def-table])
    (if (null? args)
      (pass-ret/def (list (syn-func-call id ret-s))
                    ret-e
                    ret-d)
      (match (car args)
        [(syn-meta p)
         (let*-values ([(m)
                        (gensym)]
                       [(e d)
                        (gen-func m
                                  cmd-val-t
                                  #f
                                  p
                                  ret-d)])
           (loop (cdr args)
                 (append ret-s
                         (list (syn-func-call m #f)))
                 (append ret-e
                         e)
                 d))]
        [_
         (loop (cdr args)
               (append ret-s
                       (list (car args)))
               ret-e
               ret-d)]))))

