#!/bin/sh
:; exec gosh -- $0 "$@"

(use file.util)

(define-syntax import-define
  (syntax-rules ()
    ((_ mod names ...)
     (begin
       (%import-define mod names)
       ...))))

(define-syntax %import-define
  (syntax-rules ()
    ((_ mod name)
     (define name (with-module mod name)))))

(import-define gauche.internal
               pass1
               pair-attribute-get
               pair-attribute-set!
               extended-pair?
               extended-cons
               vm-current-module)

(define (make-cenv)
  (let ((vec (make-vector 5 #f)))
    (vector-set! vec 0 (vm-current-module))
    (vector-set! vec 1 '())
    vec))

(define (safe-read port)
  (guard (exc
          (else (eof-object)))
    (read port)))

(define (attach-source-name x src-name)
  (cond ((extended-pair? x)
         (and-let* ((info (pair-attribute-get x 'source-info #f)))
           (pair-attribute-set! x 'source-info
                                (cons src-name (cdr info))))
         (attach-source-name (car x) src-name)
         (attach-source-name (cdr x) src-name))
        ((pair? x)
         (attach-source-name (car x) src-name)
         (attach-source-name (cdr x) src-name))
        (else x)))

(define (absolute-path path)
  (if (absolute-path? path)
      path
      (build-path (current-directory) path)))

(define (main args)
  (let ((path (absolute-path (cadr args)))
        (forms (port->list safe-read (current-input-port)))
        (cenv (make-cenv)))
    (define (dump-info form)
      (when (pair? form)
        (and-let* (((extended-pair? form))
                   (ap (pair-attribute-get form 'call-type #f))
                   (info (pair-attribute-get form 'source-info #f)))
          (write `(,@info ,(write-to-string (car form)) ,ap))
          (newline))
        (dump-info (car form))
        (dump-info (cdr form))))
    (attach-source-name forms path)
    (for-each (cut pass1 <> cenv) forms)
    (display "(")
    (for-each dump-info forms)
    (display ")")
    (newline)
    0))

