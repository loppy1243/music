#lang racket

(provide (all-defined-out))

;; Non-terminals
(struct syn-prog      (objs))
(struct syn-note-base (note ?acc))
(struct syn-note-c    (note-base len))
(struct syn-len       (len-base ?dot ?tie))
(struct syn-func-call (id ?args))
(struct syn-q-id      (id))
(struct syn-func-arg  (obj))
(struct syn-if-stmt   (ifs))
(struct syn-if        (test body))
(struct syn-else      (body))
(struct syn-test      (?arg1 op arg2))
(struct syn-def       (id type ?params body))
(struct syn-param     (id type))
(struct syn-cmd       (name ?arg ?rel))
(struct syn-repeat    (num body))
(struct syn-meta      (meta))
(struct syn-ret       (val))

;; Terminals
(struct syn-len-base (val))
(struct syn-type     (val))
(struct syn-id       (val))
(struct syn-func-id  (val))
(struct syn-num      (val))
(struct syn-acc      (val))
(struct syn-note     (val))
(struct syn-rest     ())
(struct syn-c        (val))
(struct syn-op       (val))
(struct syn-comment  ())
(struct syn-inc      ())
(struct syn-dec      ())
(struct syn-dot      ())

(define (syn-expr? x)
  (or (syn-note-c? x)
      (syn-func-call? x)
      (syn-if-stmt? x)
      (syn-def? x)
      (syn-cmd? x)
      (syn-repeat? x)
      (syn-meta? x)
      (syn-ret? x)
      (syn-comment? x)))

(define s-num-2
  (syn-num 2))

(define s-command-t
  (syn-type 'command))

(define s-num-t
  (syn-type 'num))

(define s-note-base-t
  (syn-type 'note-base))

(define s-len-t
  (syn-type 'len))

(define s-id-t
  (syn-type 'id))

(define s-macro-t
  (syn-type 'macro))

(define s-prog-t
  (syn-type 'prog))

(struct type (set))
(struct type-exn:mismatch exn:fail:user
                          (received expected))
(struct type-exn:invalid exn:fail:user
                         (obj))

(define (make-type-exn:mismatch t1 t2)
  (type-exn:mismatch (format "Type mismatch:\nExpected: ~a\nReceived: ~a"
                             (type->string t2)
                             (type->string t1))
                     (current-continuation-marks)
                     t1
                     t2))

(define (make-type-exn:invalid obj)
  (type-exn:invalid (format "Object << ~a >> does not have a type"
                            obj)
                    (current-continuation-marks)
                    obj))

(define (type->string t)
  (format "(type ~a)"
          (type-set t)))

(define (display-type t)
  (display (type->string t)))


(define (type-subset? t1 t2)
  (let ([s1 (type-set t1)]
        [s2 (type-set t2)])
    (subset? s1 s2)))

(define (!type-subset? t1 t2)
  (let ([s1 (type-set t1)]
        [s2 (type-set t2)])
    (or (subset? s1 s2)
        (raise (make-type-exn:mismatch t1 t2)))))

(define (type-of obj)
  (match obj
    [(? syn-note-base?) note-base-t]
    [(? syn-c?)         command-t]
    [(? syn-num?)       num-t]
    [(? syn-len?)       len-t]
    [_                  none-t]))

(define (!type-of obj)
  (let ([ret (type-of obj)])
    (if (type-subset? ret none-t)
      (raise (make-type-exn:invalid obj))
      ret)))

(define command-t
  (type (set s-command-t)))

(define num-t
  (type (set s-num-t)))

(define note-base-t
  (type (set s-note-base-t)))

(define len-t
  (type (set s-len-t)))

(define val-t
  (type (set s-note-base-t
             s-len-t
             s-num-t
             s-command-t)))

(define cmd-val-t
  (type (set s-note-base-t
             s-len-t
             s-num-t)))

(define none-t
  (type (set)))

