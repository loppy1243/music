#lang racket/unit

(require "syn-tree.rkt"
         "pass.rkt"
         "mark.rkt"
         "ret-type-param.rkt"
         "params-param.rkt"
         "run-pass-sig.rkt"
         "def-table.rkt"
         "exp-prog-sig.rkt"
         "expand-exn.rkt"
         "func-exn.rkt"
         anaphoric
         racket/match)

(import exp-prog^)
(export run-pass^)

;;; Returns a number of half-steps n such that n = 0 is A440 and n != 0 is A440 + n. Frequency is
;;; given by f(n) = 440 * 2^(n / 12)
(define (gen-note-num note ?acc)
  (let ([n (case note
             [(A) 0]
             [(B) 2]
             [(C) 3]
             [(D) 5]
             [(E) 7]
             [(F) 8]
             [(G) 10]
             [else (!raise-impossible "A note can only possibly be A-G\nFrom: gen-note-num")])]
        [?acc (if (syn-acc? ?acc)
                (case (syn-acc-val ?acc)
                  [(b)     -1]
                  [(n)      0]
                  [(|#|;|#
                          ) 1]
                  [else (!raise-impossible "An accent can only possibly be b, n, #\nFrom: gen-note-num")])
                #f)])
    (if ?acc
      (+ n ?acc)
      n)))

;;; Returns then length in 64th notes
(define (gen-len-num base ?dot ?tie)
  (+ (* (if ?dot
          3/2
          1)
        (case base
          [(w) 64]
          [(h) 32]
          [(q) 16]
          [(e) 8]
          [(s) 4]
          [(t) 2]
          [else (!raise-impossible "A len-base can only possibly be w, h, q, e, s, t\nFrom: gen-len-num")]))
     (if ?tie
       (gen-len-num (syn-len-base-val (syn-len-len-base ?tie)
                                      (syn-len-?dot ?tie)
                                      (syn-len-?tie ?tie)))
       0)))

(define (syn->integer obj n)
  (match obj
    [(syn-note-base (syn-note note)
                    ?acc)
     (if (> n 1)
       (list (gen-note-num note ?acc)
             (if ?acc
               0
               1))
       (list (gen-note-num note ?acc)))]
    [(syn-note-base (syn-rest)
                    ?acc)
     (if (> n 1)
       (list -2
             (if ?acc
               0
               1))
       (list -2))]
    [(syn-len (syn-len-base base)
              ?dot
              ?tie)
     (list (gen-len-num base ?dot ?tie))]
    [(syn-num num)
     (list num)]
    [(syn-c cmd)
     (list (case cmd
             [(K) 1]
             [(O) 2]
             [(T) 3]
             [(S) 4]
             [else (!raise-impossible "A command can only possibly be K, O, T, S\nFrom: syn->integer")]))]
    [_
     (!raise-impossible "Should only ever have a syn-note-base, syn-num, syn-cmd in a value position\nFrom: syn->integer")]))

(define (exp-rep-arg arg regs type)
  (parameterize ([current-ret-type type])
    (match arg
      [(syn-func-call (syn-func-id id)
                      ?args)
       `(,@(exp-call-func id ?args)
         (eval-mov-rval->reg ,(car regs)))]
      [(syn-q-id (syn-id id))
       (let* ([p (if-let [it (hash-ref (current-params) id #f)]
                   it
                   (raise (make-func-exn:no-param id)))]
              [p-reg (car p)]
              [p-type (cdr p)])
         (!type-subset? p-type
                        (current-ret-type))
        `((eval-mov-areg->reg ,p-reg ,(car regs))))]
      [_
       (map (λ (r v)
              `(eval-mov-int->reg ,r ,v))
            regs
            (syn->integer arg
                          (length regs)))])))

(define (exp-call-func id ?args)
  (let ([func (def-entry def-table id)])
    (match func
      [(def-table-entry func-m func-t #f)
       (when ?args
         (raise (make-func-exn:arity-mismatch id
                                              (length ?args)
                                              0)))
       (!type-subset? func-t
                      (current-ret-type))
       `((eval-call ,func-m))]
      [(def-table-entry func-m func-t params)
       (unless (or ?args
                   (= (length ?args)
                      (length params)))
         (raise (make-func-exn:arity-mismatch id
                                              (length ?args)   
                                              (length params))))
       (!type-subset? func-t
                      (current-ret-type))
       `(,@(let loop ([args ?args]
                      [params params]
                      [i 1]
                      [ret (list)])
             (if (null? args)
               ret
               (loop (cdr args)
                     (cdr params)
                     (add1 i)
                     (append ret
                             (exp-rep-arg (syn-func-arg-obj (car args))
                                          (list i)
                                          (car params))))))
          (eval-push-astk)
          ,@(apply exp-swap-arg-regs
                   (build-list (length ?args)
                               add1))
          (eval-call ,func-m)
          (eval-pop-astk))])))

(define (exp-swap-arg-regs r1 . rst)
  `(,@(map (λ (x)
             `(eval-swap-reg/areg ,x ,x))
           (cons r1 rst))))

(define def-table null)

(define (run-pass x dt)
  (set! def-table dt)
  (match x 
    [(? syn-comment?)
     (pass-del)]
    [(syn-ret (syn-func-call (syn-func-id id)
                             ?args))
     (pass-ins `(,@(exp-call-func id ?args)
                 (eval-ret)))]
    [(syn-ret obj)
     (!type-subset? (!type-of obj)
                    (current-ret-type))
     (pass-ins `((eval-mov-int->rval ,(syn->integer obj))
                 (eval-ret)))]
    [(syn-note-c base len)
     (pass-ins `(,@(exp-rep-arg base
                                (list 1 2)
                                note-base-t)
                 ,@(exp-rep-arg len
                                (list 3)
                                len-t)
                 (eval-push-astk)
                 ,@(exp-swap-arg-regs 1 2 3)
                 (eval-note/3)
                 (eval-pop-astk)))]
    [(syn-if-stmt ifs)
     (pass-ins (exp-ifs ifs))]
    [(syn-cmd name ?arg ?rel)
     (pass-ins `(,@(exp-rep-arg name
                                (list 1)
                                command-t)
                 ,@(if ?arg
                     (exp-rep-arg ?arg
                                  (list 2)
                                  cmd-val-t)
                     '((eval-mov-int->reg 2 1)))
                 (eval-mov-int->reg 3
                                    ,(cond [(syn-inc? ?rel)  1]
                                           [(syn-dec? ?rel) -1]
                                           [else             0]))
                 (eval-push-astk)
                 ,@(exp-swap-arg-regs 1 2 3)
                 (eval-cmd/3)
                 (eval-pop-astk)))]
    [(syn-func-call (syn-func-id id)
                    ?args)
     (parameterize ([current-ret-type none-t])
       (pass-ins (exp-call-func id ?args)))]
    [(syn-repeat num prog)
     (let ([loop-m (gen-mark)]
           [exit-m (gen-mark)])
       (pass-ins `((eval-push-cstk)
                   ,@(exp-rep-arg num
                                  (list 1)
                                  num-t)
                   (eval-mov-reg->recnt 1)

                   (eval-ins-mark ,loop-m)
                   (eval-jump-recnt=int 0 ,exit-m)
                   (eval-dec-recnt)
                   ,@(exp-prog prog def-table)
                   (eval-jump ,loop-m)
                   (eval-ins-mark ,exit-m)
                   (eval-pop-cstk))))]
    [_
     (pass-cont)]))

(define (exp-ifs ifs)
  (let ([if-ms (build-list (length ifs)
                           (λ (_) (gen-mark)))]
        [last-m (gen-mark)])
    (let loop ([ifs ifs]
               [if-ms if-ms]
               [ret (list)])
      (if (null? ifs)
        (append ret
                `((eval-ins-mark ,last-m)))
        (loop (cdr ifs)
              (cdr if-ms)
              (append ret
                      (exp-ifs-1 (car ifs)
                                 (car if-ms)
                                 (if (null? (cdr if-ms))
                                   #f
                                   (cadr if-ms))
                                 last-m)))))))

(define (exp-ifs-1 iff cur-m next-m last-m)
  (match iff
    [(syn-if test body)
     `((eval-ins-mark ,cur-m)
       ,@(exp-test test
                   next-m)
       ,@(exp-prog body def-table)
       (eval-jump ,last-m))]
    [(syn-else body)
     `((eval-ins-mark ,cur-m)
       ,@(exp-prog body def-table))]
    [_
     (!raise-impossible "Can only possibly have syn-if, syn-else in a syn-if-stmt list\nFrom: exp-ifs-1")]))

(define (exp-test test fail-m)
  (match test
    [(syn-test #f
               (syn-op 'not)
               (? syn-test? arg2))
     (let ([succ-m (gen-mark)])
       `(,@(exp-test arg2 succ-m)
         (eval-jump ,fail-m)
         (eval-ins-mark ,succ-m)))]
    [(syn-test #f
               (syn-op 'not)
               _)
     (!raise-impossible "NOT only operates on tests\nFrom: exp-test")]
    [(syn-test #f
               (syn-op _)
               _)
     (!raise-impossible "Only the NOT test is unary\nFrom: exp-test")]
    [(syn-test (not (? syn-test?))
               (syn-op (or 'and
                           'or
                           'xor))
               (not (? syn-test?)))
     (!raise-impossible "AND, OR, XOR only operate on tests\nFrom: exp-test")]
    [(syn-test arg1
               (syn-op 'and)
               arg2)
     `(,@(exp-test arg1 fail-m)
       ,@(exp-test arg2 fail-m))]
    [(syn-test arg1
               (syn-op 'or)
               arg2)
     (let ([cont-m (gen-mark)]
           [succ-m (gen-mark)])
       `(,@(exp-test arg1 cont-m)
         (eval-jump ,succ-m)
         (eval-ins-mark ,cont-m)
         ,@(exp-test arg2 fail-m)
         (eval-ins-mark ,succ-m)))]
    [(syn-test arg1
               (syn-op 'xor)
               arg2)
     (let ([cont-m (gen-mark)]
           [succ-m (gen-mark)])
       `(,@(exp-test arg1 cont-m)
         ,@(exp-test arg2 succ-m)
         (eval-jump ,fail-m)
         (eval-ins-mark ,cont-m)
         ,@(exp-test arg2 fail-m)
         (eval-ins-mark ,succ-m)))]
    [(syn-test (? syn-test?) _ _)
     (!raise-impossible "Value operators do not operate on tests\nFrom: exp-test")]
    [(syn-test _ _ (? syn-test?))
     (!raise-impossible "Value operators do not operate on tests\nFrom: exp-test")]
    [(syn-test arg1
               (syn-op op)
               arg2)
     `(,@(exp-rep-arg arg1
                      (list 1)
                      command-t)
       (eval-push-astk)
       ,@(exp-swap-arg-regs 1)
       (eval-cmd-val/1)
       (eval-pop-astk)
       (eval-mov-rval->reg 1)
       ,@(exp-rep-arg arg2
                      (list 2)
                      cmd-val-t)
       (,@(case op
            [(is     ==) '(eval-jump-reg!=reg 1 2)]
            [(is-not !=) '(eval-jump-reg=reg  1 2)]
            [(>)         '(eval-jump-reg<=reg 1 2)]
            [(<)         '(eval-jump-reg>=reg 1 2)]
            [(>=)        '(eval-jump-reg<reg  1 2)]
            [(<=)        '(eval-jump-reg>reg  1 2)]
            [else        (!raise-impossible "Only the operators IS, ==, IS NOT, !=, >, <, >=, <=, exist\nFrom: exp-test")])
        ,fail-m))]
    [_
     (!raise-impossible "No other syntax possible\nFrom: exp-test")]))

