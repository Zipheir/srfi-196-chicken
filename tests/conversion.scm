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

;;;; Conversion

(define (check-conversion)
  (print-header "Running conversion tests...")

  (check (range->list test-empty-range) => '())
  (check (range->list test-bool-range)  => '(#f #t))
  (check (range->list test-num-range)   => test-num-seq)

  (check (generator->list (range->generator test-num-range))
   => test-num-seq)

  (check (vector->list (range->vector test-num-range)) => test-num-seq)

  (check (range->string test-empty-range) => "")
  (let ((s "0123456789"))
    (check (range->string (string-range s)) => s))

  (let* ((vec (vector 1 3 5 7 9))
         (vrange (vector->range vec)))
    (check (range-length vrange)  => (vector-length vec))
    (check (range-first vrange)   => (vector-ref vec 0))
    (check (range-last vrange)    => (vector-ref vec 4))
    (check (range->vector vrange) => vec)
    (check (range->list (begin (vector-set! vec 0 0) vrange))
     => '(1 3 5 7 9)))
)

