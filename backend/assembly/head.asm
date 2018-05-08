;;; rval  -> rax
;;; reg1  -> r9
;;; reg2  -> r10
;;; reg3  -> r11
;;; areg1 -> r12
;;; areg2 -> r13
;;; areg3 -> r14
;;; recnt -> r15

default rel

extern snprintf
extern pow
extern system

global main

%define FDIGITS 10

;;; Save necessary registers
%macro PUSH_REGS 0
  push r9
  push r10
%endmacro

;;; Restore necessart registers
%macro POP_REGS 0
  pop r10
  pop r9
%endmacro

section .data

bfmt:     db  "/usr/bin/beep -f %.2f -l %.2f", 0x00
BSTR_LEN  equ FDIGITS*2+21+1
estr:     db  "An error was encountered", `\n`
ESTR_LEN: equ $-estr
env:      dq  3, 0, 16, 100
;;            0       2   3       5       7   8      10
key:      db  0,  0,  0,  1,  0,  0,  0,  0,  1,  0,  1, ; key
          db  0,  0, -1,  0,  0,  0,  0, -1,  0,  0,  0, ; key+11
          db  1,  0,  0,  1,  0,  1,  0,  0,  1,  0,  1, ; key+11*2
          db  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, ; key+11*3
          db -1,  0, -1,  0,  0, -1,  0, -1,  0,  0, -1, ; key+11*4
          db  0,  0,  0,  1,  0,  0,  0,  0,  1,  0,  0, ; key+11*5
          db -1,  0, -1,  0,  0,  0,  0, -1,  0,  0,  0, ; key+11*6
          db  0,  0,  0,  1,  0,  1,  0,  0,  1,  0,  1, ; key+11*7
          db  0,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0, ; key+11*8
          db  1,  0,  0,  1,  0,  1,  0,  1,  1,  0,  1, ; key+11*9
          db  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0  ; key+11*10

section .bss

bstr: resb FDIGITS*2+21+1

section .text

main:
  call start

  mov rax, 0
  ret

error:
  ;; Call sys_write(2, estr, ESTR_LEN), as printf might not work
  mov rax, 1
  mov rdi, 2
  mov rsi, estr
  mov rdx, ESTR_LEN
  syscall

  ;; Call sys_exit(1)
  mov rax, 60
  mov rdi, 1
  syscall

;;; Beep
;;; xmm0 <- freq Hertz
;;; xmm1 <- dur milliseconds
;;; Clobbers C function call
beep:
  push rbp
  mov  rbp, rsp
  and  rsp, -0x10
  mov  rdi, bstr
  mov  rsi, BSTR_LEN
  mov  rdx, bfmt
  call snprintf
  mov  rdi, bstr
  call system
  mov rsp, rbp
  pop rbp
  ret

;;; Get note frequency in Hertz
;;; r12 <- hsteps
;;; freq -> xmm0
;;; f = 440 * 2^(h/12 + Oct)
;;; Clobbers r12, xmm1, C function
note_freq:
  push     rbp
  mov      rbp,    rsp
  ; Align stack to 16 bytes
  and rsp, -0x10
  cvtsi2sd xmm1,   r12
  mov      rax,    12
  cvtsi2sd xmm0,   rax
  divsd    xmm1,   xmm0
  mov      r12,    2
  call     cmd_val
  cvtsi2sd xmm0,   rax
  addsd    xmm1,   xmm0
  mov      rax,    2
  cvtsi2sd xmm0,   rax
  call     pow
  mov      rax,    440
  cvtsi2sd xmm1,   rax
  mulsd    xmm0,   xmm1
  mov      rsp,    rbp
  pop      rbp
  ret

;;; Get note duration in milliseconds
;;; r14 <- 64th notes
;;; ms -> xmm1
;;; Clobbers r12, xmm3, xmm2
note_dur:
  cvtsi2sd xmm3, r14
  mov      r12,  3
  call     cmd_val
  cvtsi2sd xmm1, rax
  mov      r12,  4
  call     cmd_val
  cvtsi2sd xmm2, rax
  mulsd    xmm1, xmm2
  divsd    xmm3, xmm1
  mov      rax,  60000
  cvtsi2sd xmm1, rax
  mulsd    xmm1, xmm3
  ret

;;; Play note
;;; r12 <- note hsteps
;;; r13 <- use_key?
;;; r14 <- len in 64th notes
note:
  cmp r13, 0
  jz .no_key
  .key:
    push rbx
    push r12
    mov  r12, 1
    call cmd_val
    mov  r12, 11
    mul  r12
    lea  rax, [key+rax]
    pop  r12
    mov  rbx, rax
    mov  rax, 0
    mov  al,  [rbx+r12]
    cbw
    cwde
    cdqe
    add  r12, rax

    call note_freq
    call note_dur
    call beep
    pop  rbx
    ret
  .no_key:
    call note_freq
    call note_dur
    call beep
    ret

;;; r12 <- cmd number
;;; r13 <- argument
;;; r14 <- rel?
cmd:
  cmp r14, 0
  jz .abs
  jl .rel_sub
  .rel_add:
    add [env+r12*8-8], r13
    ret
  .rel_sub:
    sub [env+r12*8-8], r13
    ret
  .abs:
    mov [env+r12*8-8], r13
    ret

;;; r12 <- cmd number
cmd_val:
  mov rax, [env + r12*8 - 8]
  ret

start:
  ;; Generated code here

