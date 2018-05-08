#lang racket/unit

(require racket/match
         racket/system
         racket/vector
         racket/trace
         anaphoric
         "../mark.rkt"
         "../backend-sig.rkt")

(import)
(export backend^)

(define-struct pos (val))
(define cur-pos (pos 0))

(define (advance-pos! [v 1])
  (set! cur-pos
        (pos (+ (pos-val cur-pos)
                v))))
(define (reverse-pos! [v 1])
  (set! cur-pos
        (pos (- (pos-val cur-pos)
                v))))

(define (set-pos! v)
  (set! cur-pos
        (pos v)))

(define (get-pos)
  (pos-val cur-pos))

(define (eval-prog prog eval-this port-out)
  (set-env! 1 3)
  (set-env! 2 0)
  (set-env! 3 16)
  (set-env! 4 100)
  (push-stack! (pos -1))
  (set! prog
        (append prog
                '((eval-ret))))
  (index-marks prog)
  (let ([len (length prog)])
    (let loop ()
      (if (>= (pos-val cur-pos)
              len)
        (eval-error/impossible)
        (match (eval-this (list-ref prog
                                    (get-pos)))
          [(pos -1)
           (void)]
          [(mark m)
           (set-pos! (get-mark-pos m))
           (loop)]
          [(pos p)
           (set-pos! p)
           (loop)]
          [_
           (advance-pos!)
           (loop)])))))

(define (eval-error msg)
  (error (format "EVAL ERROR: ~a"
                 msg)))

(define (eval-error/impossible)
  (eval-error "THE UNIVERSE HAS DOUBLY IMPLODED"))

(define mark-index (make-hasheq))
(define index-mark (list))

(define regs (make-vector 3 0))
(define aregs (make-vector 3 0))
(define rval 0)
(define recnt 0)

(define env (make-vector 4 0))

(define stack (list))

(define (set-reg! n x)
  (vector-set! regs
               (sub1 n)
               x))

(define (get-reg n)
  (vector-ref regs
              (sub1 n)))

(define (get-regs)
  regs)

(define (set-areg! n x)
  (vector-set! aregs
               (sub1 n)
               x))

(define (set-aregs! a)
  (set! aregs a))

(define (get-areg n)
  (vector-ref aregs
              (sub1 n)))

(define (get-aregs)
  aregs)

(define (set-rval! x)
  (set! rval x))

(define (get-rval)
  rval)

(define (set-recnt! x)
  (set! recnt x))

(define (get-recnt)
  recnt)

(define set-env!
  (case-lambda
    [(e)
     (set! env e)]
    [(n x)
     (vector-set! env
                  (sub1 n)
                  x)]))

(define get-env
  (case-lambda
    [()
     env]
    [(n)
     (vector-ref env
                 (sub1 n))]))

(define (push-stack! obj)
  (set! stack
        (cons obj stack)))

(define (pop-stack!)
  (let ([ret (car stack)])
    (set! stack
          (cdr stack))
    ret))

(define (index-marks prog)
  (let loop ([prog prog]
             [n 0])
    (if (null? prog)
      (void)
      (begin
        (when (eq? (caar prog)
                   'eval-ins-mark)
          (index-mark! (cadar prog)
                       n))
        (loop (cdr prog)
              (add1 n))))))

(define (index-mark! mark n)
  (hash-set! mark-index
             (mark-val mark)
             n))

(define (get-mark-pos mark-name)
  (if-let [it (hash-ref mark-index mark-name #f)]
    it
    (eval-error/impossible)))

(define (eval-ret)
  (pop-stack!))

(define (eval-call mark)
  (push-stack! (pos (add1 (get-pos))))
  mark)

(define (eval-push-estk)
  (push-stack! (vector-copy (get-env)))
  (void))

(define (eval-pop-estk)
  (set-env! (pop-stack!))
  (void))

(define (eval-ins-mark mark)
  (void))

(define (eval-mov-rval->reg reg)
  (set-reg! reg
            (get-rval))
  (void))

(define (eval-mov-areg->reg areg reg)
  (set-reg! reg
            (get-areg areg))
  (void))

(define (eval-mov-int->reg reg int)
  (set-reg! reg int)
  (void))

(define (eval-jump mark)
  mark)

(define (eval-swap-reg/areg reg areg)
  (let ([tmp (get-reg reg)])
    (set-reg! reg
              (get-areg areg))
    (set-areg! areg
               tmp))
  (void))

(define (eval-mov-int->rval int)
  (set-rval! int)
  (void))

(define (eval-push-cstk)
  (push-stack! (get-recnt))
  (void))

(define (eval-pop-cstk)
  (set-recnt! (pop-stack!))
  (void))

(define (eval-mov-reg->recnt reg)
  (set-recnt! (get-reg reg))
  (void))

(define (eval-dec-recnt)
  (set-recnt! (sub1 (get-recnt)))
  (void))

(define (eval-jump-recnt=int int mark)
  (if (= (get-recnt)
         int)
    mark
    (void)))

(define (eval-jump-reg!=reg r1 r2 mark)
  (if (not (= (get-reg r1)
              (get-reg r2)))
    mark
    (void)))

(define (eval-jump-reg=reg r1 r2 mark)
  (if (= (get-reg r1)
         (get-reg r2))
    mark
    (void)))

(define (eval-jump-reg<=reg r1 r2 mark)
  (if (<= (get-reg r1)
          (get-reg r2))
    mark
    (void)))

(define (eval-jump-reg>=reg r1 r2 mark)
  (if (>= (get-reg r1)
          (get-reg r2))
    mark
    (void)))

(define (eval-jump-reg<reg r1 r2 mark)
  (if (< (get-reg r1)
         (get-reg r2))
    mark
    (void)))

(define (eval-jump-reg>reg r1 r2 mark)
  (if (> (get-reg r1)
         (get-reg r2))
    mark
    (void)))

(define (eval-push-astk)
  (push-stack! (vector-copy (get-aregs)))
  (void))

(define (eval-pop-astk)
  (set-aregs! (pop-stack!))
  (void))

(define (eval-cmd-val/1)
  (set-rval! (get-env (get-areg 1)))
  (void))

(define (eval-cmd/3)
  (let ([cmd (get-areg 1)]
        [arg (get-areg 2)]
        [rel (get-areg 3)])
    (match rel
     [0 
      (set-env! cmd arg)]
     [1 
      (set-env! cmd
                (+ (get-env cmd)
                   arg))]
     [-1 
      (set-env! cmd
                (- (get-env cmd)
                   arg))]
     [_
      (eval-error/impossible)]))
    (void))

(define (get-key)
  (case (get-env 1)
    [(0) ; A
     (hasheq 0   0 ; A
             2   0 ; B
             3   1 ; C
             5   0 ; D
             7   0 ; E
             8   1 ; F
             10  1 ; G
                  )]
    [(1) ; Bb
     (hasheq 0   0
             2  -1
             3   0
             5   0
             7  -1
             8   0
             10  0)]
    [(2) ; B
     (hasheq 0   1
             2   0
             3   1
             5   1
             7   0
             8   1
             10  1)]
    [(3) ;C
     (hasheq 0   0
             2   0
             3   0
             5   0
             7   0
             8   0
             10  0)]
    [(4) ; Db
     (hasheq 0  -1
             2  -1
             3   0
             5  -1
             7  -1
             8   0
             10 -1)]
    [(5) ; D
     (hasheq 0   0
             2   0
             3   1
             5   0
             7   0
             8   1
             10  0)]
    [(6) ; Eb
     (hasheq 0  -1
             2  -1
             3   0
             5   0
             7  -1
             8   0
             10  0)]
    [(7) ; E
     (hasheq 0   0
             2   0
             3   1
             5   1
             7   0
             8   1
             10  1)]
    [(8) ; F
     (hasheq 0   0
             2  -1
             3   0
             5   0
             7   0
             8   0
             10  0)]
    [(9) ; F#
     (hasheq 0   1
             2   0
             3   1
             5   1
             7   1
             8   1
             10  1)]
    [(10) ; G
     (hasheq 0   0
             2   0
             3   0
             5   0
             7   0
             8   1
             10  0)]
    [else
     (eval-error/impossible)]))

;;; In Hertz
(define (note-freq hs)
  (* 440
     (expt 2
           (+ (/ hs
                 12)
              (get-env 2)))))

;;; In milliseconds
(define (note-dur sf)
  (* 60000
     (/ sf
        (get-env 3)
        (get-env 4))))

(define (eval-note/3)
  (let ([note (get-areg 1)]
        [use-key (get-areg 2)]
        [dur (note-dur (get-areg 3))])
    (if (= note -2)
      (play-rest dur)
      (match use-key
        [0
         (play-note (note-freq note)
                    dur)]
        [1
         (play-note (note-freq (+ note
                                  (if-let [it (hash-ref (get-key) note #f)]
                                    it
                                    (eval-error/impossible))))
                    dur)]
        [_
         (eval-error/impossible)])))
  (void))

(define (play-rest dur)
  (sleep (/ dur 1000)))

(define (play-note freq dur)
  (with-output-to-file "/dev/null" #:exists 'truncate
    (λ ()
      (parameterize ([current-error-port (current-output-port)])
        (system* "/usr/bin/play"
                 "-n"
                 "synth"
                 (number->string (exact->inexact (/ dur 1000)))
                 "sin"
                 (number->string (exact->inexact freq)))))))
