(module (srfi 196) ()
  (import (scheme)
          (chicken base)
          (chicken module)
          (only (srfi 1) reduce unfold xcons every concatenate)
          (only (srfi 133) vector-copy string->vector)
          (srfi 145))
  (import (chicken platform))

  (register-feature! 'srfi-196)

  ;; "seedless" unfold/tabulate.
  (define (vector-unfold f len)
    (let ((res (make-vector len)))
      (let lp ((i 0))
        (cond ((= i len) res)
              (else (vector-set! res i (f i))
                    (lp (+ i 1)))))))

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
