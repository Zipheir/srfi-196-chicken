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

(module (srfi 196) ()
  (import (scheme)
          (chicken base)
          (chicken module)
          (chicken type)
          (chicken platform)
          (only (srfi 1) reduce unfold xcons every concatenate)
          (only (srfi 133) vector-copy string->vector vector-unfold)
          (srfi 145)
          (typed-records))

  (register-feature! 'srfi-196)

  (define eof-object
    (let ((*eof-object* (read (open-input-string ""))))
      (lambda () *eof-object*)))

  (define exact inexact->exact)

  ;; Constructors
  (export range numeric-range iota-range vector-range string-range
          range-append)

  ;; Predicates
  (export range range=?)

  ;; Accessors
  (export range-length range-ref range-first range-last)

  ;; Iteration
  (export range-split-at subrange range-segment range-take range-take-right
          range-drop range-drop-right range-count range-any range-every
          range-map range-map->list range-map->vector range-for-each
          range-filter-map range-filter-map->list range-filter
          range-filter->list range-reverse range-remove range-remove->list
          range-fold range-fold-right)

  ;; Searching
  (export range-index range-index-right range-take-while range-take-while-right
          range-drop-while range-drop-while-right)

  ;; Conversion
  (export range->list range->vector range->string vector->range
          range->generator)


  (include "srfi/196.scm"))
