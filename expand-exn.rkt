#lang racket

(provide (all-defined-out))

(struct expand-exn:impossible exn:fail:user
                              ())

(define (make-expand-exn:impossible msg)
  (expand-exn:impossible (format "IMPOSSIBLE: ~a"
                                 msg)
                         (current-continuation-marks)))

(define (!raise-impossible msg)
  (raise (make-expand-exn:impossible msg)))

