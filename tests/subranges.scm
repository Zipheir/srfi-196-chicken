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

;;; Subranges

(test-group "Subranges"
  (define r (numeric-range 10 30))

  (test-group "Splitting ranges"
    (test "Range r has length 20"
          20
          (range-length r))

    (let-values (((ra rb) (range-split-at r 10)))
      (test "Length of left split range is 10"
            10
            (range-length ra))

      (test "Length of right split range is remaining length of r"
            (- (range-length r) 10)
            (range-length rb))

      (test-assert "Appending ranges produced from split-at spans equivalent values."
                   (range=? eqv?
                            (range-append ra rb)
                            r)))
    )

  (test-group "Subrange behaviour"
    (test-assert "Subranging an entire range spans the same values"
                 (range=? eqv?
                          (subrange r 0 (range-length r))
                          r))

    (let ((lower 5)
          (upper 10))
      (test "Length of subrange is difference of upper and lower limits."
            (- upper lower)
            (range-length (subrange r lower upper)))

      (test-assert "Subrange spans equivalent values to dropping and then taking from a range."
                   (range=? eqv?
                            (subrange r lower upper)
                            (range-take (range-drop test-num-range lower)
                                        (- upper lower))))

      (test-assert "Subrange from 0 spans equivalent values to taking from range."
                   (range=? eqv?
                            (subrange r 0 upper)
                            (range-take r upper)))

      (test-assert "Subrange from 'n' to length spans same values as dropping first n elements."
                   (range=? eqv?
                            (subrange r lower (range-length r))
                            (range-drop r lower))))
    )

  (test-group "Taking from ranges"
    (test "Length of range is equivalent to number of elements taken."
          10
          (range-length (range-take r 10)))

    (test "Range P taking every element from Q has equal length to Q."
          (range-length r)
          (range-length
            (range-take r (range-length r))))

    (test "Taking from a range has similar semantics to taking from a list."
          (take (iota 20 10) 5)
          (range->list (range-take r 5)))

    (test "Length of range is equivalent to number of elements taken from right."
          10
          (range-length (range-take-right r 10)))

    (test "Range P taking (on the right) every element from Q has equal length to Q."
          (range-length r)
          (range-length
            (range-take-right r (range-length r))))

    (test "Taking from right has similar semantics to dropping from a list."
          (drop (iota 20 10) 15)
          (range->list (range-take-right r 5)))
    )

  (test-group "Dropping from ranges"
    ;; range-drop r n returns a range of length (range-length r) - n.
    (test "Length of dropped range is equal to difference of elements taken from length."
          (- (range-length r) 10)
          (range-length (range-drop r 10)))

    (test "Dropping every element results in empty range."
          0
          (range-length
            (range-drop r (range-length r))))

    (test "Dropping from a range has similar semantics to dropping from a list."
          (drop (iota 20 10) 15)
          (range->list (range-drop r 15)))

    ;; range-drop-right r n returns a range of length (range-length r) - n.
    (test "Range dropping n elements has length equal to difference of length and n."
          (- (range-length r) 10)
          (range-length
            (range-drop-right r 10)))

    (test "Dropping every element (from right) produces empty range."
          0
          (range-length
            (range-drop-right r (range-length r))))

    (test "Dropping (from right) has similar semantics to taking from a list."
          (take (iota 20 10) 5)
          (range->list (range-drop-right r 15)))
    )

  (test-group "Segmenting ranges"
    (test-assert "First range segment (length n) spans equivalent values to taking n from range."
                 (range=? eqv?
                          (car (range-segment r 5))
                          (range-take r 5)))

    (test-assert "Remaining range segments spans equivalent values to dropping first segment."
                 (range=? eqv?
                          (apply range-append
                                 (cdr (range-segment r 5)))
                          (range-drop r 5)))

    (test-assert "Appending all range segments produces range spanning equivalent values."
                 (range=? eqv?
                          (apply range-append (range-segment r 5))
                          r))

    (test "Sum of lengths of all 5-length segments is equal to original range length."
          (range-length r)
          (fold + 0 (map range-length (range-segment r 5))))

    ;; i.e. segments are "up-to" the argument passed in.
    (test "Sum of lengths of all 7-length segments is equal to original range length."
          (range-length r)
          (fold + 0 (map range-length (range-segment r 7))))
    )
  )

