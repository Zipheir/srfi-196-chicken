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

(test-group "Range predicates"
  (define r (numeric-range 10 30))
  (define bool-range (range 2 zero?))

  (test-group "Span of values in ranges with predicate range=?"

    (test-assert "Two ranges with zero length span equivalent values."
                 (range=? eqv? (numeric-range 0 0) (numeric-range 5 5)))

    (test-assert "Two ranges with different lengths span different values."
                 (not (range=? eqv? (numeric-range 0 0) r)))

    (test-assert "A range spans equivalent values to itself."
                 (range=? eqv? r r))

    (let ((A (numeric-range 10 30))
          (B (numeric-range 10 30)))
      (test-assert "Two numeric ranges, A and B, are not the same range."
                   (not (eq? A B)))

      (test-assert "Numeric ranges A and B span equivalent values."
                   (range=? eqv? A B)))

    (test-assert "Numeric ranges with differing lengths span different values"
                 (not (range=? eqv? r (numeric-range 10 20))))

    (let ((A (range 2 zero?))
          (B (vector-range #(#t #f))))
      (test-assert "Two boolean ranges, A and B, are not the same range"
                   (not (eq? A B)))

      (test-assert "Two boolean ranges, A and B, were constructed by different procedures."
                   (not (eq? range vector-range)))

      (test-assert "Boolean ranges A and B span equivalent values"
                   (range=? eqv? A B))

      (test-assert "Order matters when defining / comparing boolean ranges."
                   (not (range=? eqv? A (vector-range #(#f #t))))))

    (test-assert "Subranges of equivalent length and offset span equivalent values."
                 (range=? eqv?
                          (numeric-range 10 30)
                          (subrange (numeric-range 0 50) 10 30)))

    (test-assert "Ranges with differing length span differing values."
                 (not
                   (range=? eqv?
                            bool-range
                            r
                            (subrange (numeric-range 0 50) 10 30))))

    (test-assert "Ranges with same length but differing offsets span differing values."
                 (not
                   (range=? eqv?
                            r
                            (subrange (numeric-range 0 50) 11 31))))
    )

  (test-group "Predicates over the whole range"
    (test "Counting over every value in range returns length of range."
          (range-length r)
          (range-count (lambda _ #t) r))

    (test "Counting over no value in range returns 0."
          0
          (range-count (lambda _ #f) r))

    (test "range-count has similar semantics to counting over a list"
          (count even? (iota 20 10))
          (range-count even? r))

    (test "Counting across multiple ranges counts predicate successes."
          1
          (range-count (lambda (x y) y)
                       r
                       bool-range))

    (test "Counting predicates over mapped values."
          (range-length r)
          (range-count zero?
                       (range-map - r r)))

    (test-assert "Searching for any even number in (numeric-range 10 30) succeeds."
                 (range-any even? r))

    (test-assert "Searching for any always-false predicate fails."
                 (not (range-any (lambda _ #f) r)))

    (test-assert "Searching across multiple ranges succeeds."
                 (range-any (lambda (x y) y)
                            r
                            bool-range))

    (test-assert "Searching for any zero value in mapped numeric range."
                 (not (range-any zero?
                                 (range-map + r r))))

    (test-assert "Every value in numeric range is a number."
                 (range-every number? r))

    (test-assert "Not every value in numeric range is even."
                 (not (range-every even? r)))

    (test-assert "Every value tested with a boolean range fails."
                 (not (range-every (lambda (x y) y)
                                   r
                                   bool-range)))

    (test-assert "Every value from mapped difference in range is zero"
                 (range-every zero? (range-map - r r)))
    )
  )
