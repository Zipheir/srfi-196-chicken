;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(define *tests-failed* 0)

(define-syntax check
  (syntax-rules (=>)
    ((check expr => expected)
     (if (equal? expr expected)
       (begin
         (display 'expr)
         (display " => ")
         (display expected)
         (display " ; correct")
         (newline))
       (begin
         (set! *tests-failed* (+ *tests-failed* 1))
         (display "FAILED: for ")
         (display 'expr)
         (display " expected ")
         (display expected)
         (display " but got ")
         (display expr)
         (newline))))))

(define (check-report)
  (if (zero? *tests-failed*)
      (begin
       (display "All tests passed.")
       (newline))
      (begin
       (display "TESTS FAILED: ")
       (display *tests-failed*)
       (newline))))

(define (generator->list g)
  (let ((v (g)))
    (if (eof-object? v)
        '()
        (cons v (generator->list g)))))

;;;; Utility

(define (print-header message)
  (newline)
  (display ";;; ")
  (display message)
  (newline))

(define-syntax constantly
  (syntax-rules ()
    ((_ obj) (lambda _ obj))))

(define always (constantly #t))
(define never (constantly #f))

(define (range=/eqv? ra rb)
  (range=? eqv? ra rb))

(define (%range-empty? r) (zero? (range-length r)))

;;;; Test ranges

(define test-num-range (numeric-range 10 30))

(define test-num-seq (iota 20 10))

(define test-empty-range (numeric-range 0 0))

(define (square x) (* x x))

;; Produces the range {#f, #t}.
(define test-bool-range
  (range 2 (lambda (n) (not (zero? n)))))

