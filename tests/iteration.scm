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

;;;; Iteration

(test-group "Range iteration"
  (define r (numeric-range 10 30))
  (define bool-range (range 2 zero?))
  (define (square x) (* x x))

  (test-group "Mapping over ranges"
    (test-assert "Mapping add1 over numeric range"
                 (range=? eqv?
                          (range-map add1 r)
                          (numeric-range 11 31)))

    (test "Mapping over ranges has similar semantics to mapping lists."
          (map square (iota 20 10))
          (range->list (range-map square r)))

    (test-assert "Mapping sum of range with itself."
                 (range=? eqv?
                          (range-map + r r)
                          (numeric-range 20 60 2)))

    ;; range-map over ranges with unequal lengths terminates when the shortest
    ;; range is exhausted.
    (test "Mapping over ranges with unequal lengths produces range with shortest length."
          2
          (range-length
            (range-map (lambda (x b) x)
                       r
                       bool-range)))

    ;; (range-map->list f r) = (map f (range->list r))
    (test "Mapping a range to a list is equivalent to mapping a range converted to a list."
          (map not (range->list bool-range))
          (range-map->list not bool-range))

    (test "Mapping ranges to a list has similar semantics to mapping lists."
          (map + (iota 20 10) (iota 20 10))
          (range-map->list + r r))

    ;; (range-map->vector f r) = (map f (range->vector r))
    (test "Mapping a range to a vector is equivalent to ampping a range converted to a vector"
          (vector-map not (range->vector bool-range))
          (range-map->vector not bool-range))

    (let ((num-vec (list->vector (iota 20 10))))
      (test "Mapping ranges to a vector has similar semantics to mapping vectors."
            (vector-map + num-vec num-vec)
            (range-map->vector + r r)))

    (test "Filter-mapping a range with failing predicate produces empty range."
          0
          (range-length
            (range-filter-map (lambda _ #f)
                              bool-range)))

    (test-assert "Filter-mapping a range with succeeding predicate is equivalent."
                 (range=? eqv?
                          (range-filter-map identity r)
                          r))

    (test "Filter-mapping a range has similar semantics to filter-mapping a list."
          (filter-map (lambda (x) (and (even? x) x))
                      (iota 20 10))
          (range->list (range-filter-map (lambda (x) (and (even? x) x))
                                         r)))

    (define (add-if-even x y)
      (and (even? x)
           (even? y)
           (+ x y)))

    (test-assert "Filter-mapping with add-if-even on numeric range."
                 (range=? eqv?
                          (range-filter-map add-if-even r r)
                          (numeric-range 20 60 4)))

    (test "Filter-mapping to list failing predicate produces empty list."
          '()
          (range-filter-map->list (lambda _ #f) bool-range))

    (test "Filter-mapping to list produces same list as list conversion."
          (iota 20 10)
          (range-filter-map->list values r))

    (define (proc x)
      (and (even? x) x))

    (test "Filter-mapping to list has similar semantics to filter-mapping a list."
          (filter-map proc (iota 20 10))
          (range-filter-map->list proc r))


    (test "Filter-mapping to list with add-if-even creates same list as filter-map."
          (filter-map add-if-even (iota 20 10) (iota 20 10))
          (range-filter-map->list add-if-even r r))
    )

  (test-group "For-each value in range"
    (test-assert "Setting value from boolean range."
                 (let ((v #f))
                   (range-for-each (lambda (x) (set! v x))
                                   (range-reverse bool-range))
                   v))

    (test "For-each over ranges only iterates to the smallest range."
          11
          (let ((v #f))
            (range-for-each (lambda (x y) (when y (set! v x)))
                            r
                            (range-reverse bool-range))
            v))
    )

  (test-group "Filtering / removal from ranges"
    ;;; filter & remove

    (test-assert "Filtering range with successful predicate produces equivalent range."
                 (range=? eqv?
                          (range-filter (lambda _ #t) bool-range)
                          bool-range))

    (test "Filtering with failing predicate always produces empty range."
          0
          (range-length (range-filter (lambda _ #f) bool-range)))

    (test "Filtering a range has similar semantics to filtering a list."
          (filter even? (iota 20 10))
          (range->list (range-filter even? r)))

    (test "Filtering to a list with successful predicate produced equivalent list."
          (list #t #f)
          (range-filter->list (lambda _ #t) bool-range))

    (test-assert "Filtering to a list with failing predicate produces null list."
                 (null? (range-filter->list (lambda _ #f) bool-range)))

    ;; (range-filter->list pred r) = (filter pred (range->list r))
    (test "Filtering to a list has similar semantics to filtering a list."
          (filter even? (iota 20 10))
          (range-filter->list even? r))

    (test-assert "Removing with failing predicate produces the an equivalent range."
                 (range=? eqv?
                          (range-remove (lambda _ #f) bool-range)
                          bool-range))

    (test "Removing with successful predicate produces empty range."
          0
          (range-length (range-remove (lambda _ #t) bool-range)))

    (test "Removing from a range has similar semantics to removing from a list."
          (remove even? (iota 20 10))
          (range->list (range-remove even? r)))

    (test "Removing to a list with failing predicate is same as list conversion."
          (range->list bool-range)
          (range-remove->list (lambda _ #f) bool-range))

    (test-assert "Removing to a list with successful predicate produces null list."
                 (null? (range-remove->list (lambda _ #t) bool-range)))

    ;; (range-remove->list pred r) = (remove pred (range->list r))
    (test "Removing to a list has similar semantics to removing from a list."
          (remove even? (iota 20 10))
          (range-remove->list even? r))
    )

  (test-group "Folding over ranges"
    ;; (range-fold (lambda (b) (+ 1 b)) 0 r) = (range-length r)
    (test "Folding a count over a range is the length of the range."
          (range-length r)
          (range-fold (lambda (knil x) (add1 knil)) 0 r))

    ;; (range-fold proc nil r) = (fold proc nil (range->list r))
    (test "Folding a range has similar semantics to folding a list."
          (fold + 0 (iota 20 10))
          (range-fold + 0 r))

    (test "Folding over multiple ranges has similar semantics to folding over multiple lists."
          (fold + 0 (iota 20 10) (iota 20 10))
          (range-fold + 0 r r))

    ;; range-fold over ranges with unequal lengths terminates when
    ;; the shortest range is exhausted.
    (test "Folding over ranges with unequal lengths terminates when the shortest range is exhausted."
          (range-fold + 0 (range-take r
                                      (range-length bool-range)))
          (range-fold (lambda (knil x _) (+ knil x))
                      0
                      r
                      bool-range))

    ;; (range-fold-right (lambda (b) (+ 1 b)) 0 r) = (range-length r)
    (test "Folding right with a count over a range is the length of the range."
          (range-length r)
          (range-fold-right (lambda (knil _) (add1 knil)) 0 r))

    ;; (range-fold-right r proc nil) = (fold-right proc nil (range->list r))
    (test "Folding right over a range has similar semantics to folding right over a list."
          (fold-right + 0 (iota 20 10))
          (range-fold-right + 0 r))

    (test "Folding right over ranges has similar semantics to folding right over lists."
          (fold-right + 0 (iota 20 10) (iota 20 10))
          (range-fold-right + 0 r r))

    ;; range-fold-right over ranges with unequal lengths terminates when
    ;; the shortest range is exhausted.
    (test "Folding right over ranges with unequal lengths terminates when the shortest range is exhausted."
          (range-fold-right + 0 (range-take r
                                            (range-length bool-range)))
          (range-fold-right (lambda (knil x _) (+ knil x))
                            0
                            r
                            bool-range))
    )

  (test-group "Reversing ranges"
    (test "First element of reversed range is equal to last element of range."
          (range-first (range-reverse bool-range))
          (range-last bool-range))

    (test "Last element of reversed range is equal to first element of range."
          (range-last (range-reverse bool-range))
          (range-first bool-range))

    (test "Reversing a range has similar semantics to reversing a list."
          (reverse (iota 20 10))
          (range->list (range-reverse r)))

    (test-assert "Range reversal is reflective"
                 (range=? eqv?
                          r
                          (range-reverse (range-reverse r))))
    )

  (test-group "Appending ranges"
    (test "Appending zero ranges together produces empty range."
          0
          (range-length (range-append)))

    (test-assert "Appending a single range produces an equivalent range."
                 (range=? eqv?
                          r
                          (range-append r)))

    (test-assert "Appending two ranges produces second range appended to first."
                 (range=? eqv?
                          (range-append (numeric-range 10 20)
                                        (numeric-range 20 30))
                          r))

    (test-assert "Appending three ranges produces following ranges appended to first."
                 (range=? eqv?
                          (range-append (numeric-range 10 15)
                                        (numeric-range 15 20)
                                        (numeric-range 20 25)
                                        (numeric-range 25 30))
                          r))
    )
  )

