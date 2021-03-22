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

(test-group "Serching ranges"
  (define r (numeric-range 10 30))
  (define bool-range (range 2 zero?))

  (test-group "Finding indices in range."
    (test "Searching for index with successful predicate returns 0."
          0
          (range-index (lambda _ #t) r))

    (test "Searching for index with failing predicate returns #f."
          #f
          (range-index (lambda _ #f) r))

    (test "Searching for index on boolean range returns 0."
          0
          (range-index identity bool-range))

    (test "Searching for index across multiple ranges."
          1
          (range-index (lambda (x y) (and (odd? x) y))
                       r
                       (range-reverse bool-range)))

    (test "Searching right for index with successful predicate returns length - 1."
          (- (range-length r) 1)
          (range-index-right (lambda _ #t) r))

    (test "Searching right for index with failing predicate returns #f."
          #f
          (range-index-right (lambda _ #f) r))

    (test "Searching right for index on boolean range returns 0."
          0
          (range-index-right identity bool-range))

    (test "Searching right for index across multiple ranges."
          4
          (range-index-right (lambda (x y) (< (+ x y) 30))
                             r
                             r))

    (define (fifteen? n) (= n 15))

    ;; range-index and range-index-right produce the same index if pred
    ;; is only satisfied by the element at that index.
    (test "Searching for index is same regardless of direction if predicate is only satisfied by one element."
          (range-index fifteen? r)
          (range-index-right fifteen? r))

    (test "Searching for index has same semantics as searching for list index."
          (list-index fifteen? (iota 20 10))
          (range-index fifteen? r))
    )

  (test-group "Take-while from ranges"

    ;; (range-take-while always r) = r
    (test-assert "Take-while with a successful predicate returns an equivalent range"
                 (range=? eqv? (range-take-while (lambda _ #t) bool-range)
                          bool-range))

    ;; (range-take-while never r) = [empty range]
    (test "Take-while with a failing predicate returns an empty range."
          0
          (range-length (range-take-while (lambda _ #f) bool-range)))

    (define (less-than-fifteen? n) (< n 15))


    (test "Take-while on a range has similar semantics to take-while over lists."
          (take-while less-than-fifteen? (iota 20 10))
          (range->list (range-take-while less-than-fifteen? r)))

    ;; (range-drop-while always r) = [empty range]
    (test "Drop-while with a successful predicate returns an empty range."
          0
          (range-length (range-drop-while (lambda _ #t) bool-range)))

    ;; (range-drop-while never r) = r
    (test-assert "Drop-while with a failing predicate returns an equivalent range."
                 (range=? eqv?
                          (range-drop-while (lambda _ #f) bool-range)
                          bool-range))

    (test "Drop-while on a range has similar semantics to drop-while over lists."
          (drop-while less-than-fifteen? (iota 20 10))
          (range->list
            (range-drop-while less-than-fifteen? r)))

    (define (less-than-ten? n) (< n 10))

    ;; (range-append (range-take-while p r) (range-drop-while p r)) = r
    (test-assert "Appending take-while/drop-while ranges produces an equivalent range."
                 (range=? eqv?
                          (range-append (range-take-while less-than-ten? r)
                                        (range-drop-while less-than-ten? r))
                          r))

    ;; (range-take-while-right always r) = r
    (test-assert "Take-while from right with successful predicate returns equivalent range."
                 (range=? eqv?
                          (range-take-while-right (lambda _ #t) bool-range)
                          bool-range))

    ;; (range-take-while-right never r) = [empty range]
    (test "Take-while from right with failing predicate returns empty range."
          0
          (range-length
            (range-take-while-right (lambda _ #f) bool-range)))

    (define (greater-than-or-equal-fifteen? n)
      (not (less-than-fifteen? n)))

    (test "Take-while from right on numeric range."
          (iota 15 15)
          (range->list
            (range-take-while-right greater-than-or-equal-fifteen? r)))

    ;; (range-drop-while-right always r) = [empty range]
    (test "Drop-while from right with successful predicate returns empty range."
          0
          (range-length
            (range-drop-while-right (lambda _ #t) bool-range)))

    ;; (range-drop-while-right never r) = r
    (test-assert "Drop-while from right with failing predicate returns equivalent range."
                 (range=? eqv?
                          (range-drop-while-right (lambda _ #f) bool-range)
                          bool-range))

    (test "Drop-while from right has similar semantics to taking from a list."
          (take (iota 20 10) 5)
          (range->list
            (range-drop-while-right greater-than-or-equal-fifteen? r)))

    ;; (range-append (range-drop-while-right p r)
    ;;               (range-take-while-right p r)) = r
    (test-assert "Appending drop-while/takae-while produces equivalent range."
                 (range=? eqv?
                          (range-append (range-drop-while-right less-than-ten? r)
                                        (range-take-while-right less-than-ten? r))
                          r))
    )
  )
