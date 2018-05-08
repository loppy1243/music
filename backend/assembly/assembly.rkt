#lang racket/unit

(require racket/system
         racket/port
         racket/string
         "../../backend-sig.rkt"
         "../../mark.rkt")

(import)
(export backend^)

(define HEAD-FILE "backend/assembly/head.asm")

(define (get-tmp-file)
  (string-trim (with-output-to-string
                 (λ ()
                   (system* "/usr/bin/mktemp")))))

(define (eval-prog prog eval-f port-out)
  (define head-port (open-input-file HEAD-FILE))

  (define tmp-file-asm (get-tmp-file))
  (define asm-port
    (open-output-file tmp-file-asm
                      #:exists 'replace))
  (copy-port head-port asm-port)
  (close-input-port head-port)

  (for-each (λ (x)
               (display (eval-f x)
                        asm-port))
            prog)
  (display "ret\n" asm-port)
  (close-output-port asm-port)
 
  (define tmp-file-obj (get-tmp-file))
  (system* "/usr/bin/nasm"
           "-f" "elf64"
           "-o" tmp-file-obj
           tmp-file-asm)
  
  (define tmp-file-out (get-tmp-file))
  (system* "/usr/bin/gcc"
           "-o" tmp-file-out
           "-l" "m"
           tmp-file-obj)

  (define in (open-input-file tmp-file-out))
  (copy-port in port-out)
  (close-input-port in)
  
  (system* "/bin/rm"
           tmp-file-asm
           tmp-file-obj
           tmp-file-out))

(define (eval-error msg)
  (error (format "EVAL ERROR: ~a"
                 msg)))

(define (eval-error/impossible)
  (eval-error "THE UNIVERSE HAS DOUBLY IMPLODED"))

(define (eval-call mark)
  (format "call ~a\n"
          (mark-val mark)))

(define (eval-ret)
  "ret\n")

(define (eval-push-estk)
  (string-append "push QWORD [env]\n"
                 "push QWORD [env+8]\n"
                 "push QWORD [env+16]\n"
                 "push QWORD [env+24]\n"))

(define (eval-pop-estk)
  (string-append "pop QWORD [env+24]\n"
                 "pop QWORD [env+16]\n"
                 "pop QWORD [env+8]\n"
                 "pop QWORD [env]\n"))

(define (eval-push-astk)
  (string-append "push r12\n"
                 "push r13\n"
                 "push r14\n"))

(define (eval-pop-astk)
  (string-append "pop r14\n"
                 "pop r13\n"
                 "pop r12\n"))

(define (eval-ins-mark mark)
  (format "~a:\n"
          (mark-val mark)))

(define (get-reg n)
  (case n
    [(1) "r9"]
    [(2) "r10"]
    [(3) "r11"]
    [else (eval-error/impossible)]))

(define (get-areg n)
  (case n
    [(1) "r12"]
    [(2) "r13"]
    [(3) "r14"]
    [else (eval-error/impossible)]))

(define (eval-mov-rval->reg reg)
  (format "mov ~a, rax\n"
          (get-reg reg)))

(define (eval-mov-areg->reg areg reg)
  (format "mov ~a, ~a\n"
          (get-reg reg)
          (get-areg areg)))

(define (eval-mov-int->reg reg int)
  (format "mov ~a, ~a\n"
          (get-reg reg)
          int))

(define (eval-jump mark)
  (format "jmp ~a\n"
          (mark-val mark)))

(define (eval-swap-reg/areg reg areg)
  (format "xchg ~a, ~a\n"
          (get-reg reg)
          (get-areg areg)))

(define (eval-mov-int->rval int)
  (format "mov rax, ~a\n"
          int))

(define (eval-note/3)
  "call note\n")

(define (eval-cmd/3)
  "call cmd\n")

(define (eval-push-cstk)
  "push r15\n")

(define (eval-pop-cstk)
  "pop r15\n")

(define (eval-mov-reg->recnt reg)
  (format "mov r15, ~a\n"
          (get-reg reg)))

(define (eval-dec-recnt)
  "dec r15\n")

(define (eval-jump-recnt=int int mark)
  (format (string-append "cmp r15, ~a\n"
                         "je ~a\n")
          int
          (mark-val mark)))

(define (eval-cmd-val/1)
  "call cmd_val\n")

(define (eval-jump-reg!=reg r1 r2 mark)
  (format (string-append "cmp ~a, ~a\n"
                         "jne ~a\n")
          (get-reg r1)
          (get-reg r2)
          (mark-val mark)))

(define (eval-jump-reg=reg r1 r2 mark)
  (format (string-append "cmp ~a, ~a\n"
                         "je ~a\n")
          (get-reg r1)
          (get-reg r2)
          (mark-val mark)))

(define (eval-jump-reg<=reg r1 r2 mark)
  (format (string-append "cmp ~a, ~a\n"
                         "jle ~a\n")
          (get-reg r1)
          (get-reg r2)
          (mark-val mark)))

(define (eval-jump-reg>=reg r1 r2 mark)
  (format (string-append "cmp ~a, ~a\n"
                         "jge ~a\n")
          (get-reg r1)
          (get-reg r2)
          (mark-val mark)))

(define (eval-jump-reg<reg r1 r2 mark)
  (format (string-append "cmp ~a, ~a\n"
                         "jl ~a\n")
          (get-reg r1)
          (get-reg r2)
          (mark-val mark)))

(define (eval-jump-reg>reg r1 r2 mark)
  (format (string-append "cmp ~a, ~a\n"
                         "jg ~a\n")
          (get-reg r1)
          (get-reg r2)
          (mark-val mark)))

