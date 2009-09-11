#lang scheme

(require (planet williams/science/random-distributions/gaussian)
         (planet williams/science/math)
         ;; use one or the other below
         (planet williams/science/statistics)
         ;"statistics.ss"
         )

(define (test)
  (let ((data1 (build-vector
                100000
                (lambda (i)
                  (random-unit-gaussian))))
        (data2 (build-vector
                100000
                (lambda (i)
                  (random-unit-gaussian))))
        (w (build-vector
            100000
            (lambda (i)
              (expt (cos (- (* 2.0 pi (/ i 100000.0)) pi)) 2)))))
    ;; Don't time first call
    (variance data1)
    ;; 
    (time
     (for ((i (in-range 10)))
       (variance data1)))
    ;; See if explicitly saying it's a vector helps.
    ;(time
    ; (for ((i (in-range 10)))
    ;   (variance (in-vector data1))))
    ;;
    (time
     (for ((i (in-range 10)))
       (unchecked-variance data1)))
    ;; See if explicitly saying it's a vector helps.
    ;(time
    ; (for ((i (in-range 10)))
    ;   (unchecked-variance (in-vector data1))))
))

(test)