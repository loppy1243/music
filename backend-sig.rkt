#lang racket/signature

(require "mark.rkt")

(contracted [eval-prog (-> (listof list?)
                           (-> any/c
                               any/c)
                           output-port?
                           any)]
            [eval-call (-> mark?
                           any)]
            [eval-ret (-> any)]
            [eval-push-estk (-> any)]
            [eval-pop-estk (-> any)]
            [eval-push-astk (-> any)]
            [eval-pop-astk (-> any)]
            [eval-ins-mark (-> mark?
                               any)]
            [eval-mov-rval->reg (-> integer?
                                    any)]
            [eval-mov-areg->reg (-> integer?
                                    integer?
                                    any)]
            [eval-mov-int->reg (-> integer?
                                   integer?
                                   any)]
            [eval-jump (-> mark?
                           any)]
            [eval-swap-reg/areg (-> integer?
                                    integer?
                                    any)]
            [eval-mov-int->rval (-> integer?
                                    any)]
            [eval-note/3 (-> any)]
            [eval-cmd/3 (-> any)]
            [eval-push-cstk (-> any)]
            [eval-pop-cstk (-> any)]
            [eval-mov-reg->recnt (-> integer?
                                     any)]
            [eval-dec-recnt (-> any)]
            [eval-jump-recnt=int (-> integer?
                                     mark?
                                     any)]
            [eval-cmd-val/1 (-> any)]
            [eval-jump-reg!=reg (-> integer?
                                    integer?
                                    mark?
                                    any)]
            [eval-jump-reg=reg (-> integer?
                                   integer?
                                   mark?
                                   any)]
            [eval-jump-reg<=reg (-> integer?
                                    integer?
                                    mark?
                                    any)]
            [eval-jump-reg>=reg (-> integer?
                                    integer?
                                    mark?
                                    any)]
            [eval-jump-reg<reg (-> integer?
                                   integer?
                                   mark?
                                   any)]
            [eval-jump-reg>reg (-> integer?
                                   integer?
                                   mark?
                                   any)])

