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

(define (check-iteration)
  (print-header "Running iteration tests...")

  ;; Check lengths of ranges returned by range-split-at.
  (let ((n 10))
    (check (let-values (((ra rb) (range-split-at test-num-range n)))
             (list (range-length ra) (range-length rb)))
     => (list n (- (range-length test-num-range) n))))

  ;; Joining the two ranges returned by range-split-at gives the
  ;; original range.
  (check (let-values (((ra rb) (range-split-at test-bool-range 1)))
           (range=/eqv? (range-append ra rb) test-bool-range))
   => #t)

  (check (range=/eqv?
          (subrange test-bool-range 0 (range-length test-bool-range))
          test-bool-range)
   => #t)
  (let ((a 5) (b 10))
    (check (= (range-length (subrange test-num-range a b)) (- b a))
     => #t)
    (check (range=/eqv? (subrange test-num-range a b)
                        (range-take (range-drop test-num-range a) (- b a)))
     => #t)
    (check (range=/eqv? (subrange test-num-range 0 b)
                        (range-take test-num-range b))
     => #t)
    (check (range=/eqv?
            (subrange test-num-range a (range-length test-num-range))
            (range-drop test-num-range a))
     => #t))

  ;; range-take r n returns a range of length n.
  (check (range-length (range-take test-num-range 10)) => 10)
  (check (range-length
          (range-take test-num-range (range-length test-num-range)))
   => (range-length test-num-range))
  (check (range->list (range-take test-num-range 5))
   => (take test-num-seq 5))

  ;; range-take-right r n returns a range of length n.
  (check (range-length (range-take-right test-num-range 10)) => 10)
  (check (range-length
          (range-take-right test-num-range (range-length test-num-range)))
   => (range-length test-num-range))
  (check (range->list (range-take-right test-num-range 5))
   => (drop test-num-seq 15))

  ;; range-drop r n returns a range of length (range-length r) - n.
  (check (range-length (range-drop test-num-range 10))
   => (- (range-length test-num-range) 10))
  (check (range-length
          (range-drop test-num-range (range-length test-num-range)))
   => 0)
  (check (range->list (range-drop test-num-range 15))
   => (drop test-num-seq 15))

  ;; range-drop-right r n returns a range of length (range-length r) - n.
  (check (range-length (range-drop-right test-num-range 10))
   => (- (range-length test-num-range) 10))
  (check (range-length
          (range-drop-right test-num-range (range-length test-num-range)))
   => 0)
  (check (range->list (range-drop-right test-num-range 15))
   => (take test-num-seq 5))

  (check (range=/eqv? (car (range-segment test-num-range 5))
                      (range-take test-num-range 5))
   => #t)
  (check (range=/eqv? (apply range-append
                             (cdr (range-segment test-num-range 5)))
                      (range-drop test-num-range 5))
   => #t)
  (check (range=/eqv? (apply range-append (range-segment test-num-range 5))
                      test-num-range)
   => #t)
  (check (fold + 0 (map range-length (range-segment test-num-range 5)))
   => (range-length test-num-range))
  (check (fold + 0 (map range-length (range-segment test-num-range 7)))
   => (range-length test-num-range))

  (check (range-count always test-num-range) => (range-length test-num-range))
  (check (range-count never test-num-range)  => 0)
  (check (range-count even? test-num-range)  => (count even? test-num-seq))
  (check (range-count (lambda (x y) y) test-num-range test-bool-range)
   => 1)
  (check (range-count (lambda (x y) (zero? (+ x y)))
                      test-num-range
                      (range-map - test-num-range))
   => (range-length test-num-range))

  (check (range-any even? test-num-range) => #t)
  (check (range-any never test-num-range) => #f)
  (check (range-any (lambda (x y) y) test-num-range test-bool-range)
   => #t)
  (check (range-any (lambda (x y) (zero? (+ x y)))
                    test-num-range
                    test-num-range)
   => #f)

  (check (range-every number? test-num-range) => #t)
  (check (range-every even? test-num-range)   => #f)
  (check (range-every (lambda (x y) y) test-num-range test-bool-range)
   => #f)
  (check (range-every (lambda (x y) (zero? (+ x y)))
                      test-num-range
                      (range-map - test-num-range))
   => #t)

  ;;; map, filter-map, & for-each

  (check (range=/eqv? (range-map (lambda (x) (+ 1 x)) test-num-range)
                      (numeric-range 11 31))
   => #t)
  (check (equal? (range->list (range-map square test-num-range))
                 (map square test-num-seq))
   => #t)
  (check (range=/eqv? (range-map + test-num-range test-num-range)
                      (numeric-range 20 60 2))
   => #t)
  ;; range-map over ranges with unequal lengths terminates when
  ;; the shortest range is exhausted.
  (check (range=/eqv?
          (range-map (lambda (x _) x) test-num-range test-bool-range)
          (range-take test-num-range (range-length test-bool-range)))
   => #t)

  ;; (range-map->list f r) = (map f (range->list r))
  (check (equal? (range-map->list not test-bool-range)
                 (map not (range->list test-bool-range)))
   => #t)
  (check (equal? (range-map->list + test-num-range test-num-range)
                 (map + test-num-seq test-num-seq))
   => #t)

  ;; (range-map->vector f r) = (map f (range->vector r))
  (check (equal? (range-map->vector not test-bool-range)
                 (vector-map not (range->vector test-bool-range)))
   => #t)
  (let ((num-vec (list->vector test-num-seq)))
    (check (equal? (range-map->vector + test-num-range test-num-range)
                   (vector-map + num-vec num-vec))
     => #t))

  (check (%range-empty? (range-filter-map never test-bool-range)) => #t)
  (check (range=/eqv? (range-filter-map values test-num-range)
                      test-num-range)
   => #t)
  (check (equal?
          (range->list (range-filter-map (lambda (x) (and (even? x) x))
                                         test-num-range))
          (filter-map (lambda (x) (and (even? x) x)) test-num-seq))
   => #t)
  (let ((proc (lambda (x y) (and (even? x) (even? y) (+ x y)))))
    (check (range=/eqv? (range-filter-map proc test-num-range test-num-range)
                        (numeric-range 20 60 4))
     => #t))

  (check (range-filter-map->list never test-bool-range) => '())
  (check (equal? (range-filter-map->list values test-num-range)
                 test-num-seq)
   => #t)
  (check (equal?
          (range-filter-map->list (lambda (x) (and (even? x) x))
                                  test-num-range)
          (filter-map (lambda (x) (and (even? x) x)) test-num-seq))
   => #t)
  (let ((proc (lambda (x y) (and (even? x) (even? y) (+ x y)))))
    (check (equal? (range-filter-map->list proc
                                           test-num-range
                                           test-num-range)
                   (filter-map proc test-num-seq test-num-seq))
     => #t))

  (check (let ((v #f))
           (range-for-each (lambda (x) (set! v x)) test-bool-range)
           v)
   => #t)
  (check (let ((v #f))
           (range-for-each (lambda (x y) (when y (set! v x)))
                           test-num-range
                           test-bool-range)
           v)
   => 11)

  ;;; filter & remove

  (check (range=/eqv? (range-filter always test-bool-range)
                      test-bool-range)
   => #t)
  (check (%range-empty? (range-filter never test-bool-range)) => #t)
  (check (equal? (range->list (range-filter even? test-num-range))
                 (filter even? test-num-seq))
   => #t)

  (check (range-filter->list always test-bool-range) => '(#f #t))

  (check (null? (range-filter->list never test-bool-range)) => #t)

  ;; (range-filter->list pred r) = (filter pred (range->list r))
  (check (equal? (range-filter->list even? test-num-range)
                 (filter even? test-num-seq))
   => #t)

  (check (range=/eqv? (range-remove never test-bool-range)
                      test-bool-range)
   => #t)
  (check (%range-empty? (range-remove always test-bool-range))
   => #t)
  (check (equal? (range->list (range-remove even? test-num-range))
                 (remove even? test-num-seq))
   => #t)

  (check (equal? (range-remove->list never test-bool-range)
                 (range->list test-bool-range))
   => #t)

  (check (null? (range-remove->list always test-bool-range)) => #t)

  ;; (range-remove->list pred r) = (remove pred (range->list r))
  (check (equal? (range-remove->list even? test-num-range)
                 (remove even? test-num-seq))
   => #t)

  ;; (range-fold (lambda (b) (+ 1 b)) 0 r) = (range-length r)
  (check (= (range-fold (lambda (b _) (+ b 1)) 0 test-num-range)
            (range-length test-num-range))
   => #t)

  ;; (range-fold proc nil r) = (fold proc nil (range->list r))
  (check (equal? (range-fold + 0 test-num-range)
                 (fold + 0 test-num-seq))
   => #t)

  (check (= (range-fold + 0 test-num-range test-num-range)
            (fold + 0 test-num-seq test-num-seq))
   => #t)

  ;; range-fold over ranges with unequal lengths terminates when
  ;; the shortest range is exhausted.
  (check (= (range-fold (lambda (s x _) (+ s x))
                        0
                        test-num-range
                        test-bool-range)
            (range-fold + 0 (range-take test-num-range
                                        (range-length test-bool-range))))
   => #t)

  ;; (range-fold-right (lambda (b) (+ 1 b)) 0 r) = (range-length r)
  (check (= (range-fold-right (lambda (b _) (+ b 1)) 0 test-num-range)
            (range-length test-num-range))
   => #t)

  ;; (range-fold-right r proc nil) = (fold-right proc nil (range->list r))
  (check (equal? (range-fold-right + 0 test-num-range)
                 (fold-right + 0 test-num-seq))
   => #t)

  (check (= (range-fold-right + 0 test-num-range test-num-range)
            (fold-right + 0 test-num-seq test-num-seq))
   => #t)

  ;; range-fold-right over ranges with unequal lengths terminates when
  ;; the shortest range is exhausted.
  (check (= (range-fold-right (lambda (s x _) (+ s x))
                              0
                              test-num-range
                              test-bool-range)
            (range-fold-right + 0 (range-take test-num-range
                                              (range-length
                                               test-bool-range))))
   => #t)

  (check (eqv? (range-first (range-reverse test-bool-range))
               (range-last test-bool-range))
   => #t)

  (check (eqv? (range-last (range-reverse test-bool-range))
               (range-first test-bool-range))
   => #t)

  (check (equal? (range->list (range-reverse test-num-range))
                 (reverse test-num-seq))
   => #t)

  (check (%range-empty? (range-append)) => #t)
  (check (range->list (range-append test-bool-range)) => '(#f #t))
  (check (range=/eqv? (range-append (numeric-range 10 20)
                                    (numeric-range 20 30))
                      test-num-range)
   => #t)
  (check (range=/eqv? (range-append (numeric-range 10 15)
                                    (numeric-range 15 20)
                                    (numeric-range 20 25)
                                    (numeric-range 25 30))
                      test-num-range)
   => #t)
)

