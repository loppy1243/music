#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (except-out (all-defined-out)
                     main))
(define (main)
  (with-input-from-file "test.music"
    (thunk
      (let loop ((a (lex (current-input-port))))
        (unless (and (eq? a 'eof)
                     (begin (display 'eof)
                            (newline)
                            #t))
          (write a)
          (newline)
          (loop (lex (current-input-port))))))))

(define-tokens music-tokens
  (;; Numeric
   num

   ;; Identifiers
   id
   func-id

   ;; Notes
   note
   acc-mark
   len-base
   
   ;; Commands
   cmd
   
   ;; Types
   type
   
   ;; If ops
   op))

(define-empty-tokens music-empty-tokens
  (;; Whitespace
   ws EOF
   
   ;; Braces
   lbrace rbrace
   lparen rparen
   lrepeat rrepeat
 
   ;; Commands
   plus minus
   ret

   ;; Notes
   rest
   dot
   
   ;; Substitution
   comma
   bar

   ;; If statements
   if
   elif
   else
   
   ;; If ops
   is
   not
   is-not
   
   ;; Definitions
   define
   
   ;; Types
   colon
   
   ;; Comments
   comment))

(define-lex-abbrevs
  [identifier (:+ (:or alphabetic
                       numeric
                       #\_
                       #\!
                       #\?))])

(define (lex port)
  (lex-f port))

(define lex-f
  (lexer-src-pos
    [(eof)
     (token-EOF)]
    [(:: #\;
         (:* (:- any-char
                 #\Newline))
         #\Newline)
     (token-comment)]
    [(:+ whitespace)
     (token-ws)]
    [#\{
     (token-lbrace)]
    [#\}
     (token-rbrace)]
    [#\(
     (token-lparen)]
    [#\)
     (token-rparen)]
    ["|:"
     (token-lrepeat)]
    [":|"
     (token-rrepeat)]
    [(:: (:? #\-)
         (:+ numeric))
     (token-num (string->number lexeme))]
    [(:or #\K
          #\O
          #\T
          #\S)
     (token-cmd (string->symbol lexeme))]
    ["RET"
     (token-ret)]
    [#\+
     (token-plus)]
    [#\-
     (token-minus)]
    [(:or #\A #\B #\C #\D #\E #\F #\G)
     (token-note (string->symbol lexeme))]
    ["R"
     (token-rest)]
    [(:or #\b #\# #\n)
     (token-acc-mark (string->symbol lexeme))]
    ; FIXME
    [(:or #\w #\h #\q #\e #\s #\t)
     (token-len-base (string->symbol lexeme))]
    [#\.
     (token-dot)]
    [#\,
     (token-comma)]
    [#\|
     (token-bar)]
    ["IF"
     (token-if)]
    ["ELIF"
     (token-elif)]
    ["ELSE"
     (token-else)]
    [(:or "AND"
          "OR"
          "XOR"
          "<="
          ">="
          "=="
          "!="
          #\<
          #\>)
     (token-op (string->symbol (string-downcase lexeme)))]
    [(:& "IS"
         (:+ whitespace)
         "NOT")
     (token-is-not)]
    ["IS"
     (token-is)]
    ["NOT"
     (token-not)]
    ["DEFINE"
     (token-define)]
    [(:or "COMMAND"
          "NUM"
          "LEN"
          "KEY")
     (token-type (string->symbol (string-downcase lexeme)))]
    [#\:
     (token-colon)]
    [(:: #\' identifier)
     (token-id (string->symbol (substring lexeme
                                          1
                                          (string-length lexeme))))]
    [(:: #\, identifier)
     (token-func-id (string->symbol (substring lexeme
                                               1
                                               (string-length lexeme))))]
    [any-char
      (begin
        (display "LEX FAIL: ")
        (displayln lexeme))]))

