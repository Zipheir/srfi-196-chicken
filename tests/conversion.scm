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

(test-group "Range conversions"
  (test "Converting empty range to list is '()"
        '()
        (range->list (numeric-range 0 0)))

  (test "Converting boolean range to list is '(#t #f)"
        (list #t #f)
        (range->list (range 2 zero?)))

  (test "Converting (numeric-range 10 30) to list is equal to (iota 20 10)"
        (iota 20 10)
        (range->list (numeric-range 10 30)))

  (test "Converting range to generator produces correct set of values."
        (iota 20 10)
        (generator->list
          (range->generator (numeric-range 10 30))))

  (test "Converting range to vector produces correct set of values."
        (iota 20 10)
        (vector->list (range->vector (numeric-range 10 30))))

  (test "Converting empty range to string produces empty string."
        ""
        (range->string (numeric-range 0 0)))

  (test-group "String range conversions"
    (define s "0123456789")

    (test "Converting a range made from a string to a string is reflective."
          s
          (range->string (string-range s)))

    ;; First we create the range from the string
    (define srange (string-range s))

    ;; Then we set the string that we created the range from
    (string-set! s 0 #\c)

    (test "Changing the original string doesn't affect ranges made from that string."
          (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
          (range->list srange))
    )

  (test-group "Vector range conversions"
    (define vec (vector 1 3 5 7 9))

    (test "Converting a range made from a vector to a vector is reflective."
          vec
          (range->vector (vector-range vec)))


    ;; First we create the range from the vector
    (define vrange (vector-range vec))

    ;; Then we set the vector that we created the range from
    (vector-set! vec 0 #\c)

    (test "Changing the original vector doesn't affect ranges made from that vector."
          (list 1 3 5 7 9)
          (range->list vrange))
    )
  )

