#lang racket

(require parser-tools/yacc
         "lex.rkt"
         "syn-tree.rkt")

(provide (all-defined-out))

(define (make-lexer lexer port)
  (thunk (lexer port)))

(define (parse lexer)
  (parse-f lexer))

(define parse-f
  (parser
    (tokens music-tokens
            music-empty-tokens)
    (start prog)
    (end EOF)
    (error (λ (tok-ok? tok-name tok-val start end)
             (printf "ERROR: ~a -- ~a\n"
                     start
                     end)
             (printf "tok-ok?: ~a\n"
                     tok-ok?)
             (printf "tok-name: ~a\n"
                     tok-name)
             (printf "tok-val: ~a\n"
                     tok-val)))
    (src-pos)
    (grammar
      [prog               ((opt-ws prog-body)
                           (syn-prog $2))]

      [prog-body          ((command opt-ws opt-prog-body)
                           (cons $1 $3))
                          ((stmt opt-ws opt-prog-body)
                           (cons $1 $3))
                          ((s-id opt-ws opt-prog-body)
                           (cons $1 $2))
                          ((comment opt-ws opt-prog-body)
                           (cons (syn-comment) $3))]

      [opt-prog-body      ((prog-body)
                           $1)
                          (()
                           (list))]
      
      [func-prog          ((opt-ws func-prog-body)
                           (syn-prog $2))]
      
      [func-prog-body     ((command opt-ws func-prog-body)
                           (cons $1 $3))
                          ((stmt/no-def opt-ws opt-func-prog-body)
                           (cons $1 $3))
                          ((block opt-ws opt-func-prog-body)
                           (cons $1 $3))
                          ((s-id opt-ws opt-func-prog-body)
                           (cons $1 $2))
                          ((comment opt-ws opt-func-prog-body)
                           (cons (syn-comment) $3))]

      [opt-func-prog-body ((func-prog-body)
                           $1)
                          (()
                           (list))]
      
      [block              ((lbrace opt-ws opt-prog opt-ws rbrace)
                           $3)]

      [opt-prog           ((prog)
                           $1)
                          (()
                           (syn-prog (list)))]

      [opt-ws             ((ws)
                           'nothing)
                          (()
                           'nothing)]
      
      [s-id               ((id)
                           (syn-id $1))]
      
      [s-num              ((num)
                           (syn-num $1))]
      
      [s-note             ((note)
                           (syn-note $1))
                          ((rest)
                           (syn-rest))]

      [s-acc              ((acc-mark)
                           (syn-acc $1))]
      
      [s-inc              ((plus)
                           (syn-inc))]
      
      [s-dec              ((minus)
                           (syn-dec))]
      
      [s-func-id          ((func-id)
                           (syn-func-id $1))]
      
      [s-dot              ((dot)
                           (syn-dot))]
      
      [s-cmd              ((cmd)
                           (syn-c $1))]
      
      [s-len-base         ((len-base)
                           (syn-len-base $1))]
      
      [s-op               ((op)
                           (syn-op $1))
                          ((is-not)
                           (syn-op 'is-not))
                          ((is)
                           (syn-op 'is))]
      
      [s-not              ((not)
                           (syn-op 'not))]
      
      [s-type             ((type)
                           (case $1
                             [(command) command-t]
                             [(num)     num-t]
                             [(len)     len-t]
                             [(key)     note-base-t]
                             [(none)    none-t]
                             [else      (error "IMPOSSIBLE: No other type exists\nFrom: parse/s-type")]))]
      
      [command            ((note-c)
                           $1)
                          ((cmd-td rel opt-c-arg)
                           (syn-cmd $1 $3 $2))
                          ((ret ret-arg)
                           (syn-ret $2))
                          ((repeat)
                           $1)]

      [opt-c-arg          ((c-arg)
                           $1)
                          (()
                           #f)]
      
      [cmd-td             ((s-cmd)
                           $1)
                          ((replace)
                           $1)]
      
      [ret-arg            ((lparen opt-ws ret-val opt-ws rparen)
                           $3)]
      
      [ret-val            ((note-base)
                           $1)
                          ((len)
                           $1)
                          ((s-num)
                           $1)
                          ((replace)
                           $1)
                          ((s-cmd)
                           $1)
                          ((s-id)
                           $1)
                          ((s-func-id)
                           $1)
                          ((block)
                           $1)]
      
      [c-arg              ((lparen opt-ws c-val opt-ws rparen)
                           $3)]
      
      [c-val              ((note-base)
                           $1)
                          ((len)
                           $1)
                          ((s-num)
                           $1)
                          ((replace)
                           $1)]
      
      [repeat             ((repeat-num opt-ws repeat-body)
                           (syn-repeat $1 $3))
                          ((opt-ws repeat-body)
                           (syn-repeat s-num-2 $2))]
      
      [repeat-body        ((lrepeat opt-ws opt-prog opt-ws rrepeat)
                           $3)]
      
      [repeat-num         ((s-num)
                           $1)
                          ((replace)
                           $1)
                          ((lparen opt-ws repeat-num opt-ws rparen)
                           $4)]
     
      [note-c             ((note-base-td len-td)
                           (syn-note-c $1 $2))
                          ((lparen opt-ws note-c opt-ws rparen)
                           $4)]
      
      [note-base-td       ((note-base)
                           $1)
                          ((replace)
                           $1)]
      
      [len-td             ((len)
                           $1)
                          ((replace)
                           $1)]
      
      [note-base          ((s-note opt-s-acc)
                           (syn-note-base $1 $2))]

      [opt-s-acc          ((s-acc)
                           $1)
                          (()
                           #f)]

      [rel                ((s-inc)
                           $1)
                          ((s-dec)
                           $1)
                          (()
                           #f)]

      [func-call          ((bar opt-ws s-func-id/replace opt-paren-args opt-ws bar)
                           (syn-func-call $2 $3))]

      [s-func-id/replace  ((s-func-id)
                           $1)
                          ((replace)
                           $1)]

      [opt-paren-args     ((paren-args)
                           $1)
                          (()
                           #f)]

      [paren-args         ((lparen opt-ws args opt-ws rparen)
                           $3)]

      [q-id               ((bar opt-ws s-id opt-ws bar)
                           (syn-q-id $3))]

      [replace            ((func-call)
                           $1)
                          ((q-id)
                           $1)
                          ((meta)
                           $1)]
      
      [meta               ((bar block bar)
                           (syn-meta $2))]
      
      [len                ((s-len-base opt-s-dot--opt-tie)
                           (apply syn-len $1 $2))]

      [opt-s-dot--opt-tie ((s-dot opt-tie)
                           (list $1 $2))
                          ((tie)
                           (list #f $1))
                          (()
                           (list #f #f))]

      [opt-tie            ((tie)
                           $1)
                          (()
                           #f)]

      [tie                ((minus len)
                           $2)]

;;                  Don't remember if this  \__/  is actually necessary...
      [args               ((arg opt-ws comma ws args)
                           (cons $1 $5))
                          ((arg)
                           (list $1))]

      [arg                ((s-cmd)
                           (syn-func-arg $1))
                          ((s-num)
                           (syn-func-arg $1))
                          ((note-base)
                           (syn-func-arg $1))
                          ((len)
                           (syn-func-arg $1))
                          ((replace)
                           $1)]
     
      [stmt/no-def        ((if-stmt)
                           (syn-if-stmt $1))]
      
      [stmt               ((stmt/no-def)
                           $1)
                          ((def-stmt)
                           $1)]
      
      [if-stmt            ((if ws if-test ws block opt-ws opt-if-elses)
                           (cons (syn-if $3 $5)
                                 $7))]

      [opt-if-elses       ((if-elses)
                           $1)
                          (()
                           (list (syn-else (syn-prog (list)))))]
      
      [if-elses           ((elif ws if-test opt-ws block opt-ws opt-if-elses)
                           (cons (syn-if $3 $5)
                                 $7))
                          ((else opt-ws block)
                           (list (syn-else $3)))]
      
      [if-test            ((if-var ws s-op ws c-val)
                           (syn-test $1 $3 $5))
                          ((s-not ws if-paren-test)
                           (syn-test #f $1 $3))
                          ((if-paren-test)
                           $1)]
      
      [if-var             ((s-cmd)
                           $1)
                          ((replace)
                           $1)]
      
      [if-paren-test      ((lparen opt-ws if-test opt-ws rparen)
                           $3)]
      
      [def-stmt           ((define ws s-func-id opt-params opt-ws def-type opt-ws block)
                           (syn-def $3 $6 $4 $8))]
      
      [opt-params         ((paren-params)
                           $1)
                          ((opt-ws)
                           #f)]
      
      [def-type           ((colon s-type)
                           $2)
                          ((opt-ws)
                           none-t)]
    
      [paren-params       ((lparen opt-ws params opt-ws rparen)
                           $3)]
      
      [params             ((param opt-ws comma ws params)
                           (cons $1 $5))
                          ((param)
                           (list $1))]
     
      [param              ((s-id opt-ws colon opt-ws s-type)
                           (syn-param $1 $5))])))
