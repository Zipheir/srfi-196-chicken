(module (srfi 196) ()
  ;; Constructors
  (range numeric-range iota-range vector-range string-range range-append

   ;; Predicates
   range range=?

   ;; Accessors
   range-length range-ref range-first range-last

   ;; Iteration
   range-split-at subrange range-segment range-take range-take-right
   range-drop range-drop-right range-count range-any range-every
   range-map range-map->list range-map->vector range-for-each
   range-filter-map range-filter-map->list range-filter
   range-filter->list range-reverse range-remove range-remove->list
   range-fold range-fold-right

   ;; Searching
   range-index range-index-right range-take-while range-take-while-right
   range-drop-while range-drop-while-right

   ;; Conversion
   range->list range->vector range->string vector->range
   range->generator
   )

  (import (scheme)
          (chicken base)
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


  (include "srfi/196.scm"))
