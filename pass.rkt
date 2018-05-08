#lang racket

(require "def-table.rkt"
         "syn-tree.rkt")

(provide (all-defined-out))

(struct pass-del ())
(struct pass-cont ())
(struct pass-ret (syn-expr eval-expr))
(struct pass-rep (syn-expr))
(struct pass-ins (eval-expr))
(struct pass-ins/del (eval-expr))
(struct pass-def (defs))
(struct pass-del/def (defs))
(struct pass-ret/def (syn-expr eval-expr defs))
(struct pass-rep/def (syn-expr defs))
(struct pass-ins/def (eval-expr defs))
(struct pass-ins/del/def (eval-expr defs))

(struct pass-exn:invalid-ret exn:fail:user
                             (val))

(define (make-pass-exn:invalid-ret val)
  (pass-exn:invalid-ret (format "Invalid return value << ~a >> from function called from do-passes"
                                val)
                        (current-continuation-marks)
                        val))

(define (pass-val? x)
  (or (pass-del? x)
      (pass-cont? x)
      (pass-ret? x)
      (pass-rep? x)
      (pass-ins? x)
      (pass-ins/def? x)
      (pass-def? x)
      (pass-del/def? x)
      (pass-ret/def? x)
      (pass-rep/def? x)
      (pass-ins/def? x)
      (pass-ins/del/def? x)))

(define-syntax do-passes
  (syntax-rules ()
    [(_ prog dt f fs ...)
     (let loop1 ([p (syn-prog-objs prog)]
                 [flst (list f fs ...)]
                 [def-table dt]
                 [ret (list)])
       (if (null? flst)
         (values (syn-prog p)
                 ret
                 def-table)
         (let loop2 ([plst p]
                     [def-table def-table]
                     [retp (list)]
                     [rete (list)])
           (if (null? plst)
             (loop1 retp
                    (cdr flst)
                    def-table
                    (append ret
                            rete))
             (match ((car flst) (car plst) def-table)
               [(pass-del)
                (loop2 (cdr plst)
                       def-table
                       retp
                       rete)]
               [(pass-cont)
                (loop2 (cdr plst)
                       def-table
                       (append retp
                               (list (car plst)))
                       rete)]
               [(pass-ret se ee)
                (loop2 (cdr plst)
                       def-table
                       (append retp
                               se)
                       (append rete
                               ee))]
               [(pass-rep se)
                (loop2 (cdr plst)
                       def-table
                       (append retp
                               se)
                       rete)]
               [(pass-ins ee)
                (loop2 (cdr plst)
                       def-table
                       (append retp
                               (list (car plst)))
                       (append rete
                               ee))]
               [(pass-ins/del ee)
                (loop2 (cdr plst)
                       def-table
                       retp
                       (append rete
                               ee))]
               [(pass-def defs)
                (loop2 (cdr plst)
                       defs
                       (append retp
                               (list (car plst)))
                       rete)]
               [(pass-del/def defs)
                (loop2 (cdr plst)
                       defs
                       retp
                       rete)]
               [(pass-ret/def se ee defs)
                (loop2 (cdr plst)
                       defs
                       (append retp
                               se)
                       (append rete
                               ee))]
               [(pass-rep/def se defs)
                (loop2 (cdr plst)
                       defs
                       (append retp
                               se)
                       rete)]
               [(pass-ins/def ee defs)
                (loop2 (cdr plst)
                       defs
                       (append retp
                               (list (car plst)))
                       (append rete
                               ee))]
               [(pass-ins/del/def ee defs)
                (loop2 (cdr plst)
                       defs
                       retp
                       (append rete
                               ee))]
               [x
                (raise (make-pass-exn:invalid-ret x))])))))]))


