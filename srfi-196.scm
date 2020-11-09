(module (srfi 196)
        (range numeric-range vector-range string-range range-append
         iota-range range? range=? range-length range-ref range-first
         range-last subrange range-segment range-split-at range-take
         range-take-right range-drop range-drop-right range-count
         range-map->list range-for-each range-fold range-fold-right
         range-any range-every range-filter->list range-remove->list
         range-reverse range-map range-map->vector range-filter
         range-remove range-filter-map range-filter-map->list
         range-index range-index-right range-take-while
         range-drop-while range-take-while-right
         range-drop-while-right vector->range range->string
         range->list range->generator range->vector)

  (import (scheme)
          (chicken base)
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

  (include "srfi/196.scm"))
