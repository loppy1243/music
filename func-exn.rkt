#lang racket

(provide (all-defined-out))

(struct func-exn:arity-mismatch exn:fail:user
                                (func-id received expected))
(struct func-exn:no-param exn:fail:user
                          (param-id))
(struct func-exn:defined exn:fail:user
                         (func-id))
(struct func-exn:undefined exn:fail:user
                           (func-id))

(define (make-func-exn:arity-mismatch func-id received expected)
  (func-exn:arity-mismatch (format "Arity mismatch in call to function << ~a >>:\nExpected: ~a\nReceived: ~a"
                                   func-id
                                   expected
                                   received)
                           (current-continuation-marks)
                           func-id
                           received
                           expected))

(define (make-func-exn:no-param param-id)
  (func-exn:no-param (format "ID << ~a >> in replacement is not a defined parameter"
                             param-id)
                     (current-continuation-marks)
                     param-id))

(define (make-func-exn:defined func-id)
  (func-exn:defined (format "Function << ~a >> is already defined"
                            func-id)
                    (current-continuation-marks)
                    func-id))

(define (make-func-exn:undefined func-id)
  (func-exn:undefined (format "Function << ~a >> is undefined"
                              func-id)
                      (current-continuation-marks)
                      func-id))

