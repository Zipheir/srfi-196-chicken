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

;;;; Predicates

(define (check-predicates)
  (print-header "Running predicate tests...")

  (check (range=? eqv? (numeric-range 0 0) (numeric-range 5 5))  => #t)
  (check (range=? eqv? (numeric-range 0 0) test-num-range)       => #f)
  (check (range=? eqv? test-num-range test-num-range)            => #t)
  (check (range=? eqv? test-num-range (numeric-range 10 30))     => #t)
  (check (range=? eqv? test-num-range (numeric-range 10 20))     => #f)
  (check (range=? eqv? test-bool-range (vector-range #(#f #t))) => #t)
  (check (range=? eqv? test-bool-range (vector-range #(#t #f))) => #f)
  (check (range=? eqv?
                  test-num-range
                  (numeric-range 10 30)
                  (subrange (numeric-range 0 50) 10 30))
   => #t)
  (check (range=? eqv?
                  test-bool-range
                  (numeric-range 10 30)
                  (subrange (numeric-range 0 50) 10 30))
   => #f)
  (check (range=? eqv?
                  test-num-range
                  (numeric-range 11 31)
                  (subrange (numeric-range 0 50) 10 30))
   => #f)
)
