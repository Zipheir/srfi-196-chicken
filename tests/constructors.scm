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

(test-group "Range construction"

  (test-group "Numeric range construction."
    (test "(numeric-range 1 1) is empty."
          0
          (range-length (numeric-range 1 1)))

    (test "(numeric-range -5 -1) to list is (iota 4 -5)"
          (iota 4 -5)
          (range->list (numeric-range -5 -1)))

    (test "(numeric-range 1 -5 -1) to list is (iota 6 1 -1)"
          (iota 6 1 -1)
          (range->list (numeric-range 1 -5 -1)))

    (test "(numeric-range 4/3 16/3) to list is (iota 4 4/3)"
          (iota 4 4/3)
          (range->list (numeric-range 4/3 16/3)))

    (test "(numeric-range 0 9 4) to list is (iota 3 0 4)"
          (iota 3 0 4)
          (range->list (numeric-range 0 9 4)))

    (test "(numeric-range 0 10 -1) is empty."
          0
          (range-length (numeric-range 0 10 -1)))

    (test "(numeric-range 0 -10) is empty."
          0
          (range-length (numeric-range 0 -10)))

    (test "(numeric-range 5 1 -1) to list is (iota 4 5 -1)"
          (iota 4 5 -1)
          (range->list (numeric-range 5 1 -1)))

    (test "(numeric-range -2 2) to list is (iota 4 -2)"
          (iota 4 -2)
          (range->list (numeric-range -2 2)))

    (test "(numeric-range 2 -2 -1) to list is (iota 4 2 -1)"
          (iota 4 2 -1)
          (range->list (numeric-range 2 -2 -1)))

    (test "(numeric-range -4 -8 -1) to list is (iota 4 -4 -1)"
          (iota 4 -4 -1)
          (range->list (numeric-range -4 -8 -1))   )

    (test "(numeric-range -1 -4 -2/3) to list is (iota 5 -1 -2/3)"
          (iota 5 -1 -2/3)
          (range->list (numeric-range -1 -4 -2/3)))
    )

  (test-group "Iota range construction."
    (test-assert "iota range with step 0 spans equivalent values to (range 10 (lambda _ 0))"
                 (range=? eqv?
                          (iota-range 10 0 0)
                          (range 10 (lambda (_) 0))))

    (test "iota range of zero length is empty."
          0
          (range-length (iota-range 0)))

    (test "(iota-range 10) to list is (iota 10)"
          (iota 10)
          (range->list (iota-range 10)))

    (test "(iota-range 10) to list is (iota 10 0)"
          (iota 10 0)
          (range->list (iota-range 10 0)))

    (test "(iota-range 10 0 1) to list is (iota 10 0 1)"
          (iota 10 0 1)
          (range->list (iota-range 10 0 1)))

    (test "(iota-range 10 10 2) to list is (iota 10 10 2)"
          (iota 10 10 2)
          (range->list (iota-range 10 10 2)))

    (test "(iota-range 10 0 -1) to list is (iota 10 0 -1)"
          (iota 10 0 -1)
          (range->list (iota-range 10 0 -1)))

    (test "(iota-range 10 5 -2) to list is (iota 10 5 -2)"
          (iota 10 5 -2)
          (range->list (iota-range 10 5 -2)))

    (test "(iota-range 10 1/2) to list is (iota 10 1/2)"
          (iota 10 1/2)
          (range->list (iota-range 10 1/2)))
    )

  (test-group "Vector range construction."
    (define vec (vector 1 3 5 7 9))

    (test "Range from vector produces range of same length as vector."
          (vector-length vec)
          (range-length (vector-range vec)))

    (test "First element in range is same as 0th element in vector."
          (vector-ref vec 0)
          (range-first (vector-range vec)))

    (test "Last element in range is same as (len-1)th element in vector."
          (vector-ref vec (sub1 (vector-length vec)))
          (range-last (vector-range vec)))

    (test "Converting range back to vector produces equivalent vector."
          vec
          (range->vector (vector-range vec)))
    )

  (test-group "String range construction."
    (define s "0123456789")

    (test "Range from string produces range of same length as string"
          (string-length s)
          (range-length (string-range s)))

    (test "First element in range is same as 0th character in string."
          (string-ref s 0)
          (range-first (string-range s)))

    (test "Last element in range is same as (len-1)th character in string."
          (string-ref s (sub1 (string-length s)))
          (range-last (string-range s)))

    (test "Range contains same list of characters as converting string to list."
          (string->list s)
          (range->list (string-range s)))
    )
  )
