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

;;;; Searching

(define (check-searching)
  (print-header "Running search tests...")

  (check (range-index always test-num-range) => 0)
  (check (range-index never test-num-range)  => #f)
  (check (range-index values test-bool-range) => 1)
  (check (range-index (lambda (x y) (and (odd? x) y))
                      test-num-range
                      test-bool-range)
   => 1)

  (check (eqv? (range-index-right always test-num-range)
               (- (range-length test-num-range) 1))
   => #t)
  (check (range-index-right never test-num-range)  => #f)
  (check (range-index-right values test-bool-range) => 1)
  (check (range-index-right (lambda (x y) (< (+ x y) 30))
                            test-num-range
                            test-num-range)
   => 4)

  ;; range-index and range-index-right produce the same index if pred
  ;; is only satisfied by the element at that index.
  (let ((fifteen? (lambda (n) (= n 15))))
    (check (= (range-index fifteen? test-num-range)
              (range-index-right fifteen? test-num-range)
              (list-index fifteen? test-num-seq))
     => #t))

  ;; (range-take-while always r) = r
  (check (range=/eqv? (range-take-while always test-bool-range)
                      test-bool-range)
   => #t)

  ;; (range-take-while never r) = [empty range]
  (check (%range-empty? (range-take-while never test-bool-range)) => #t)

  (let ((pred (lambda (n) (< n 15))))
    (check (range->list (range-take-while pred test-num-range))
     => (take-while pred test-num-seq)))

  ;; (range-drop-while always r) = [empty range]
  (check (%range-empty? (range-drop-while always test-bool-range)) => #t)

  ;; (range-drop-while never r) = r
  (check (range=/eqv? (range-drop-while never test-bool-range)
                      test-bool-range)
   => #t)

  (let ((pred (lambda (n) (< n 15))))
    (check (range->list (range-drop-while pred test-num-range))
     => (drop-while pred test-num-seq)))

  ;; (range-append (range-take-while p r) (range-drop-while p r)) = r
  (let ((pred (lambda (n) (< n 10))))
    (check (range=/eqv?
            (range-append (range-take-while pred test-num-range)
                          (range-drop-while pred test-num-range))
            test-num-range)
     => #t))

  ;; (range-take-while-right always r) = r
  (check (range=/eqv? (range-take-while-right always test-bool-range)
                      test-bool-range)
   => #t)

  ;; (range-take-while-right never r) = [empty range]
  (check (%range-empty? (range-take-while-right never test-bool-range)) => #t)

  (let ((pred (lambda (n) (>= n 15))))
    (check (range->list (range-take-while-right pred test-num-range))
     => (iota 15 15)))

  ;; (range-drop-while-right always r) = [empty range]
  (check (%range-empty? (range-drop-while-right always test-bool-range)) => #t)

  ;; (range-drop-while-right never r) = r
  (check (range=/eqv? (range-drop-while-right never test-bool-range)
                      test-bool-range)
   => #t)

  (let ((pred (lambda (n) (>= n 15))))
    (check (range->list (range-drop-while-right pred test-num-range))
     => (take test-num-seq 5)))

  ;; (range-append (range-drop-while-right p r)
  ;;               (range-take-while-right p r)) = r
  (let ((pred (lambda (n) (< n 10))))
    (check (range=/eqv?
            (range-append (range-drop-while-right pred test-num-range)
                          (range-take-while-right pred test-num-range))
            test-num-range)
     => #t))
)
