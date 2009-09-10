#lang scheme

;;; Running Statistics

;;; (struct statistics (m M-n S-n))
;;;   n : exact-nonnegative-integer? = 0
;;;   M-n : real? = 0
;;;   S-n : real? = 0
(define-struct statistics
  ((n #:auto)
   (M-n #:auto)
   (S-n #:auto))
  #:mutable
  #:auto-value 0)

;;; (statistics-reset! statistics) -> void?
;;;   statistics : statistics?
(define (statistics-reset! statistics)
  (set-statistics-n! statistics 0))

;;; (statistics-tally! statistics x) -> void?
;;;   statistics : statistics?
;;;   x : real?
(define (statistics-tally! statistics x)
  (let ((n (add1 (statistics-n statistics))))
    (set-statistics-n! statistics n)
    (if (= n 1)
        (begin
          (set-statistics-M-n! statistics (exact->inexact x))
          (set-statistics-S-n! statistics 0.0))
        (let* ((M-old (statistics-M-n statistics))
               (M-new (+ M-old (/ (- x M-old) n)))
               (S-old (statistics-S-n statistics))
               (S-new (+ S-old (* (- x M-old) (- x M-new)))))
          (set-statistics-M-n! statistics M-new)
          (set-statistics-S-n! statistics S-new)))))

;;; (statistics-mean statistics) -> real?
;;;   statistics : statistics?
(define (statistics-mean statistics)
  (if (> (statistics-n statistics) 0)
      (statistics-M-n statistics)
      0.0))

;;; (statistics-variance statistics) -> (>=/c 0.0)
;;; statistics : statistics?
(define (statistics-variance statistics)
  (if (> (statistics-n statistics) 1)
      (/ (statistics-S-n statistics)
         (sub1 (statistics-n statistics)))
      0.0))

;;; (statistics-standard-deviation statistics) -> (>=/c 0.0)
;;;   statistics : statistics?
(define (statistics-standard-deviation statistics)
  (sqrt (statistics-variance statistics)))

;;; Mean and Standard Deviation and Variance

;;; (mean data) -> real?
;;;   data : sequence-of-real?
(define (mean data)
  (for/fold ((m-old 0.0))
            ((i (in-naturals))
             (x data))
    (+ m-old (/ (- x m-old) (add1 i)))))

;;; (compute-variance data mean) -> exact-nonnegative-integer?
;;;                                 (>=/c 0.0)
;;;   data : sequence-of-real?
;;;   mean : real?
(define (compute-variance data mean)
  (for/fold ((i 0)
             (v-old 0.0))
            ((x data))
    (let* ((n (add1 i))
           (delta (- x mean))
           (v-new (+ v-old (/ (- (* delta delta) v-old) (add1 i)))))
      (values n v-new))))

;;; (variance-with-fixed-mean data mean) -> (>=/c 0.0)
;;;   data : sequence-of-real?
;;;   mean : real?
(define (variance-with-fixed-mean data mean)
  (let-values
      (((n V)
        (compute-variance data mean)))
    V))

;;; (standard-deviation-with-fixed-mean data mean) -> (>=/c 0.0)
;;;   data : sequence-of-real?
;;;   mean : real?
(define (standard-deviation-with-fixed-mean data mean)
  (let-values
      (((n V)
        (compute-variance data mean)))
    (sqrt V)))

;;; (variance data mean) -> (>=/c 0.0)
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
(define (variance data (mean (mean data)))
  (let-values
      (((n V)
        (compute-variance data mean)))
    (* V (/ n (sub1 n)))))

;;; (standard-deviation data mean) -> (>=/c 0.0)
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
(define (standard-deviation data (mean (mean data)))
  (let-values
      (((n V)
        (compute-variance data mean)))
    (sqrt (* V (/ n (sub1 n))))))

;;; (sum-of-squares data mean) -> (>=/c 0.0)
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
(define (sum-of-squares data (mean (mean data)))
  (for/fold ((tss-old 0.0))
            ((x data))
    (let ((delta (- x mean)))
      (+ tss-old (* delta delta)))))

;;; Absolute Deviation

;;; (absolute-deviation data mean) -> (>=/c 0.0)
;;;   data : sequence-of-real?
;;;   mean : real? (mean data)
(define (absolute-deviation data (mean (mean data)))
  (let-values
      (((n S)
        (for/fold ((i 0)
                   (sum 0.0))
                  ((x data))
          (let ((delta (abs (- x mean))))
            (values (add1 i) (+ sum delta))))))
    (/ S n)))

;;; Higher Moments (Skewness and Kurtosis)

;;; (skew data mean sd) -> real?
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
;;;   sd : (>=/c 0.0) = (standard-deviation data)
(define skew
  (case-lambda
    ((data mean sd)
     (for/fold ((skew 0.0))
               ((i (in-naturals))
                (x data))
       (let ((delta (/ (- x mean) sd)))
         (+ skew (/ (- (* delta delta delta) skew) (add1 i))))))
    ((data)
     (let* ((mean (mean data))
            (sd (standard-deviation data mean)))
       (skew data mean sd)))))

;;; (kurtosis data mean sd) -> real?
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
;;;   sd : (>=/c 0.0) = (standard-deviation data)
(define kurtosis
  (case-lambda
    ((data mean sd)
     (let ((avg
            (for/fold ((avg 0.0))
                      ((i (in-naturals))
                       (x data))
              (let ((delta (/ (- x mean) sd)))
                (+ avg (/ (- (* delta delta delta delta) avg) (add1 i)))))))
       (- avg 3.0)))
    ((data)
     (let* ((mean (mean data))
            (sd (standard-deviation data mean)))
       (kurtosis data mean sd)))))

;;; Autocorrelation

;;; (lag-1-autocorrelation data mean) -> real?
;;;   data : nonempty-sequence-of-real?
;;;   mean : real? = (mean data)
(define (lag-1-autocorrelation data (mean (mean data)))
  (let-values
      (((x-prev Q V)
        (for/fold ((x-prev 0)
                   (q-old 0.0)
                   (v-old 0.0))
                  ((i (in-naturals))
                   (x data))
          (if (= i 0)
              (let ((delta (- x mean)))
                (values x 0.0 (* delta delta)))
              (let* ((delta0 (- x-prev mean))
                     (delta1 (- x mean))
                     (q-new (+ q-old (/ (- (* delta0 delta1) q-old) (add1 i))))
                     (v-new (+ v-old (/ (- (* delta1 delta1) v-old) (add1 i)))))
                (values x q-new v-new))))))
    (/ Q V)))

;;; Covariance

;;; (covariance data1 data2 mean1 mean2) -> real?
;;;   data1 : sequence-of-real?
;;;   data2 : sequence-of-real?
;;;   mean1 : real? = (mean data1)
;;;   mean2 : real? = (mean data2)
(define (covariance data1 data2 (mean1 (mean data1)) (mean2 (mean data2)))
  (let-values
      (((n covariance)
        (for/fold ((i 0)
                   (covariance 0.0))
                  ((x1 data1)
                   (x2 data2))
          (let* ((delta1 (- x1 mean1))
                 (delta2 (- x2 mean2))
                 (covariance-new
                  (+ covariance (/ (- (* delta1 delta2) covariance) (add1 i)))))
            (values (add1 i) covariance-new)))))
    (* covariance (/ n (sub1 n)))))

;;; Correlation

;;; (correlation data1 data2) -> (>=/c 0.0)
;;;  data1 : nonempty-sequence-of-real?
;;;  data2 : nonempty-sequence-of-real?
(define (correlation data1 data2)
  (let-values
      (((sum-xsq sum-ysq sum-cross mean-x mean-y)
        (for/fold ((sum-xsq 0.0)
                   (sum-ysq 0.0)
                   (sum-cross 0.0)
                   (mean-x 0.0)
                   (mean-y 0.0))
                  ((i (in-naturals))
                   (x data1)
                   (y data2))
          (if (= i 0)
              (values 0.0 0.0 0.0 x y)
              (let* ((ratio (/ i (+ i 1.0)))
                     (delta-x (- x mean-x))
                     (delta-y (- y mean-y)))
                (values (+ sum-xsq (* delta-x delta-x ratio))
                        (+ sum-ysq (* delta-y delta-y ratio))
                        (+ sum-cross (* delta-x delta-y ratio))
                        (+ mean-x (/ (delta-x (+ i 1.0))))
                        (+ mean-y (/ (delta-y (+ i 1.0))))))))))
    (/ sum-cross (* (sqrt sum-xsq) (sqrt sum-ysq)))))

;;; Weighted Samples

;;; (weighted-mean weights data) -> real?
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
(define (weighted-mean weights data)
  (let-values
      (((W M)
        (for/fold ((w-old 0.0)
                   (m-old 0.0))
                  ((i (in-naturals))
                   (w weights)
                   (x data))
          (if (> w 0)
              (let* ((w-new (+ w-old w))
                     (m-new (+ m-old (* (- x m-old) (/ w w-new)))))
                (values w-new m-new))
              (values w-old m-old)))))
    M))

;;; (compute-weighted-variance weights data wmean) -> exact-nonnegative-integer>
;;;                                                   real?
;;;                                                   real?
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real?
(define (compute-weighted-variance weights data wmean)
  (for/fold ((i 0)
             (w-old 0.0)
             (wv-old 0.0))
            ((w weights)
             (x data))
    (if (> w 0)
        (let* ((delta (- x wmean))
               (w-new (+ w-old w))
               (wv-new (+ wv-old (* (- (* delta delta) wv-old) (/ w w-new)))))
          (values (add1 i) w-new wv-new))
        (values (add1 i) w-old wv-old))))

;;; (compute-factor weights) -> real?
;;;   weights : sequence-of-real?
(define (compute-factor weights)
  (let-values
      (((a b)
        (for/fold ((a-old 0.0)
                   (b-old 0.0))
                  ((w weights))
          (if (> w 0)
              (values (+ a-old w) (+ b-old (* w w)))
              (values a-old b-old)))))
    (/ (* a a) (- (* a a) b))))

;;; (weighted-variance-with-fixed-mean weights data wmean) -> (>=/c 0.0)
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real?
(define (weighted-variance-with-fixed-mean weights data wmean)
  (let-values
      (((n W wvariance)
        (compute-weighted-variance weights data wmean)))
    wvariance))

;;; (weighted-standard-deviation-with-fixed-mean weights data wmean) -> (>=/c 0.0)
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real?
(define (weighted-standard-deviation-with-fixed-mean weights data wmean)
  (let-values
      (((n W wvariance)
        (compute-weighted-variance weights data wmean)))
    (sqrt wvariance)))


;;; (weighted-variance weights data wmean) -> (>=/c 0.0)
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real? = (weighted-mean weights data)
(define (weighted-variance weights data (wmean (weighted-mean weights data)))
  (let-values
      (((n W wvariance)
        (compute-weighted-variance weights data wmean)))
    (let ((scale (compute-factor weights)))
      (* scale wvariance))))

;;; (weighted-standard-deviation weights data wmean) -> (>=/c 0.0)
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real? = (weighted-mean weights data)
(define (weighted-standard-deviation weights data (wmean (weighted-mean weights data)))
  (let-values
      (((n W wvariance)
        (compute-weighted-variance weights data wmean)))
    (let ((scale (compute-factor weights)))
      (sqrt (* scale wvariance)))))

;;; (weighted-sum-of-squares weights data wmean) -> (>=/c 0.0)
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real? = (weighted-mean weights data)
(define (weighted-sum-of-squares weights data (wmean (weighted-mean weights data)))
  (for/fold ((wtss-old 0.0))
            ((w weights)
             (x data))
    (if (> w 0)
        (let ((delta (- x wmean)))
          (+ wtss-old (* w delta delta)))
        wtss-old)))

;;; (weighted-absolute-deviation weights data wmean) -> (>=/c 0.0)
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real? = (weighted-mean weights data)
(define (weighted-absolute-deviation weights data (wmean (weighted-mean weights data)))
  (let-values
      (((W wabsdev)
        (for/fold ((w-old 0.0)
                   (wabsdev-old 0.0))
                  ((w weights)
                   (x data))
          (if (> w 0)
              (let* ((delta (abs (- x wmean)))
                     (w-new (+ w-old w))
                     (wabsdev-new (+ wabsdev-old (* (- delta wabsdev-old) (/ w w-new)))))
                (values w-new wabsdev-new))
              (values w-old wabsdev-old)))))
    wabsdev))
      

;;; (weighted-skew weights data wmean wsd) -> real?
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real? = (weighted-mean weights data)
;;;   wsd : (>=/c 0.0) = (weighted-standard-deviation weights data)
(define weighted-skew
  (case-lambda
    ((weights data wmean wsd)
     (let-values
         (((W wskew)
           (for/fold ((w-old 0.0)
                      (wskew-old 0.0))
                     ((w weights)
                      (x data))
             (if (> w 0)
                 (let* ((delta (/ (- x wmean) wsd))
                        (w-new (+ w-old w))
                        (wskew-new (+ wskew-old (* (- (* delta delta delta) wskew-old)
                                                   (/ w w-new)))))
                   (values w-new wskew-new))
                 (values w-old wskew-old)))))
       wskew))
    ((weights data)
     (let* ((wmean (weighted-mean weights data))
            (wsd (weighted-standard-deviation weights data wmean)))
       (weighted-skew weights data wmean wsd)))))

;;; (weighted-kurtosis weights data wmean wsd) -> real?
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real? = (weighted-mean weights data)
;;;   wsd : (>=/c 0.0) = (weighted-standard-deviation weights data)
(define weighted-kurtosis
  (case-lambda
    ((weights data wmean wsd)
     (let-values
         (((W wavg)
           (for/fold ((w-old 0.0)
                      (wavg-old 0.0))
                     ((w weights)
                      (x data))
             (if (> w 0)
                 (let* ((delta (/ (- x wmean) wsd))
                        (w-new (+ w-old w))
                        (wavg-new (+ wavg-old (* (- (* delta delta delta delta) wavg-old)
                                                 (/ w w-new)))))
                   (values w-new wavg-new))
                 (values w-old wavg-old)))))
       (- wavg 3.0)))
    ((weights data)
     (let* ((wmean (weighted-mean weights data))
            (wsd (weighted-standard-deviation weights data wmean)))
       (weighted-kurtosis weights data wmean wsd)))))

;;; Maximum and Minimum Values

;;; (minimum-maximum-and-indices data) -> real?
;;;                                       real?
;;;                                       exact-nonnegative-integer?
;;;                                       exact-nonnegative-integer?
;;;   data : nonempty-sequence-of-real?
(define (minimum-maximum-and-indices data)
  (for/fold ((min +inf.0)
             (max -inf.0)
             (min-ndx -1)
             (max-ndx -1))
            ((i (in-naturals))
             (x data))
    (let ((min-new (if (< x min) x min))
          (max-new (if (> x max) x max))
          (min-ndx-new (if (< x min) i min-ndx))
          (max-ndx-new (if (> x max) i max-ndx)))
      (values min-new max-new min-ndx-new max-ndx-new))))

;;; (minimum-maximum data) -> real?
;;;                           real?
;;;   data : nonempty-sequence-of-real?
(define (minimum-maximum data)
  (let-values (((min max min-ndx max-ndx)
                (minimum-maximum-and-indices data)))
    (values min max)))

;;; (minimum data) -> real?
;;;   data : nonempty-sequence-of-real?
(define (minimum data)
  (let-values (((min max min-ndx max-ndx)
                (minimum-maximum-and-indices data)))
    min))

;;; (maximum data) -> real?
;;;   data : nonempty-sequence-of-real?
(define (maximum data)
  (let-values (((min max min-ndx max-ndx)
                (minimum-maximum-and-indices data)))
    max))

;;; (minimum-maximum-index data) -> exact-nonnegative-integer?
;;;                                 exact-nonnegative-integer?
;;;   data : nonempty-sequence-of-real?
(define (minimum-maximum-index data)
  (let-values (((min max min-ndx max-ndx)
                (minimum-maximum-and-indices data)))
    (values min-ndx max-ndx)))

;;; (minimum-index data) -> exact-nonnegative-integer?
;;;   data : nonempty-sequence-of-real?
(define (minimum-index data)
  (let-values (((min max min-ndx max-ndx)
                (minimum-maximum-and-indices data)))
    min-ndx))

;;; (maximum-index data) -> exact-nonnegative-integer?
;;;   data : nonempty-sequence-of-real?
(define (maximum-index data)
  (let-values (((min max min-ndx max-ndx)
                (minimum-maximum-and-indices data)))
    max-ndx))

;;; Median and Quantiles

(define (median-from-sorted-data sorted-data)
  (let* ((n (vector-length sorted-data))
         (lhs (quotient (- n 1) 2))
         (rhs (quotient n 2)))
    (if (= lhs rhs)
        (vector-ref sorted-data lhs)
        (/ (+ (vector-ref sorted-data lhs)
              (vector-ref sorted-data rhs))
           2.0))))

(define (quantile-from-sorted-data sorted-data f)
  (let* ((n (vector-length sorted-data))
         (index (* f (- n 1)))
         (lhs (inexact->exact (truncate index)))
         (delta (- index lhs)))
    (if (= lhs (- n 1))
        (vector-ref sorted-data lhs)
        (+ (* (- 1.0 delta) (vector-ref sorted-data lhs))
           (* delta (vector-ref sorted-data (+ lhs 1)))))))

;;; Module Contracts

(provide
 (rename-out (mean unchecked-mean)
             (variance-with-fixed-mean unchecked-variance-with-fixed-mean)
             (standard-deviation-with-fixed-mean unchecked-standard-deviation-with-fixed-mean)
             (variance unchecked-variance)
             (standard-deviation unchecked-standard-deviation)
             (sum-of-squares unchecked-sum-of-squares)
             (absolute-deviation unchecked-absolute-deviation)
             (skew unchecked-skew)
             (kurtosis unchecked-kurtosis)
             (lag-1-autocorrelation unchecked-lag-1-autocorrelation)
             (covariance unchecked-covariance)
             (correlation unchecked-correlation)
             (weighted-mean unchecked-weighted-mean)
             (weighted-variance-with-fixed-mean unchecked-weighted-variance-with-fixed-mean)
             (weighted-standard-deviation-with-fixed-mean unchecked-weighted-standard-deviation-with-fixed-mean)
             (weighted-variance unchecked-weighted-variance)
             (weighted-standard-deviation unchecked-weighted-standard-deviation)
             (weighted-sum-of-squares unchecked-weighted-sum-of-squares)
             (weighted-absolute-deviation unchecked-weighted-absolute-deviation)
             (weighted-skew unchecked-weighted-skew)
             (weighted-kurtosis unchecked-weighted-kurtosis)
             (minimum-maximum unchecked-minimum-maximum)
             (minimum unchecked-minimum)
             (maximum unchecked-maximum)
             (minimum-maximum-index unchecked-minimum-maximum-index)
             (minimum-index unchecked-minimum-index)
             (maximum-index unchecked-maximum-index)
             (median-from-sorted-data unchecked-median-from-sorted-data)
             (quantile-from-sorted-data unchecked-quantile-from-sorted-data)
             ))

(define sequence-of-real?
  (flat-named-contract
   "sequence-of-real?"
   (lambda (s)
     (and (sequence? s)
          (let/ec exit
            (for ((x s))
              (unless (real? x)
                (exit #f)))
            #t)))))

(define nonempty-sequence-of-real?
  (flat-named-contract
   "nonempty-sequence-of-real?"
   (lambda (s)
     (and (sequence? s)
          (let ((empty? #t))
            (let/ec exit
              (for ((x s))
                (set! empty? #f)
                (unless (real? x)
                  (exit #f)))
              (not empty?)))))))

(define nonempty-sorted-vector-of-real?
  (flat-named-contract
   "nonempty-sorted-vector-of-real?"
   (lambda (v)
     (and (vector v)
          (let ((empty? #t))
            (let/ec exit
              (for/fold ((x-old -inf.0))
                        ((x (in-vector v)))
                (set! empty? #f)
                (when (or (not (real? x))
                          (< x x-old))
                  (exit #f))
                x)
              (not empty?)))))))

(provide/contract
 ;;; Running Statistics
 (statistics?
  (-> any/c boolean?))
 (make-statistics
  (-> statistics?))
 (statistics-reset!
  (-> statistics? void?))
 (statistics-tally!
  (-> statistics? real? void?))
 (statistics-n
  (-> statistics? exact-nonnegative-integer?))
 (statistics-mean
  (-> statistics? real?))
 (statistics-variance
  (-> statistics? (>=/c 0.0)))
 (statistics-standard-deviation
  (-> statistics? (>=/c 0.0)))
 ;;; Sequence Statistics
 (mean
  (-> sequence-of-real? real?))
 (variance-with-fixed-mean
  (-> sequence-of-real? real? (>=/c 0.0)))
 (standard-deviation-with-fixed-mean
  (-> sequence-of-real? real? (>=/c 0.0)))
 (variance
  (->* (sequence-of-real?) (real?) (>=/c 0.0)))
 (standard-deviation
  (->* (sequence-of-real?) (real?) (>=/c 0.0)))
 (sum-of-squares
  (->* (sequence-of-real?) (real?) (>=/c 0.0)))
 (absolute-deviation
  (->* (sequence-of-real?) (real?) (>=/c 0.0)))
 (skew
  (case-> (-> sequence-of-real? real? (>=/c 0.0) real?)
          (-> sequence-of-real? real?)))
 (kurtosis
  (case-> (-> sequence-of-real? real? (>=/c 0.0) real?)
          (-> sequence-of-real? real?)))
 (lag-1-autocorrelation
  (->* (nonempty-sequence-of-real?) (real?) real?))
 (covariance
  (->* (sequence-of-real? sequence-of-real?)
       (real? real?)
       real?))
 (correlation
  (-> nonempty-sequence-of-real? nonempty-sequence-of-real? (>=/c 0.0)))
 (weighted-mean
  (-> sequence-of-real? sequence-of-real? real?))
 (weighted-variance-with-fixed-mean
  (-> sequence-of-real? sequence-of-real? real? (>=/c 0.0)))
 (weighted-standard-deviation-with-fixed-mean
  (-> sequence-of-real? sequence-of-real? real? (>=/c 0.0)))
 (weighted-variance
  (->* (sequence-of-real? sequence-of-real?) (real?) (>=/c 0.0)))
 (weighted-standard-deviation
  (->* (sequence-of-real? sequence-of-real?) (real?) (>=/c 0.0)))
 (weighted-sum-of-squares
  (->* (sequence-of-real? sequence-of-real?) (real?) (>=/c 0.0)))
 (weighted-absolute-deviation
  (->* (sequence-of-real? sequence-of-real?) (real?) (>=/c 0.0)))
 (weighted-skew
  (case-> (-> sequence-of-real? sequence-of-real? real? (>=/c 0.0) real?)
          (-> sequence-of-real? sequence-of-real? real?)))
 (weighted-kurtosis
  (case-> (-> sequence-of-real? sequence-of-real? real? (>=/c 0.0) real?)
          (-> sequence-of-real? sequence-of-real? real?)))
 (minimum-maximum
  (-> nonempty-sequence-of-real? (values real? real?)))
 (minimum
  (-> nonempty-sequence-of-real? real?))
 (maximum
  (-> nonempty-sequence-of-real? real?))
 (minimum-maximum-index
  (-> nonempty-sequence-of-real? (values exact-nonnegative-integer? exact-nonnegative-integer?)))
 (minimum-index
  (-> nonempty-sequence-of-real? exact-nonnegative-integer?))
 (maximum-index
  (-> nonempty-sequence-of-real? exact-nonnegative-integer?))
 (median-from-sorted-data
  (-> nonempty-sorted-vector-of-real? real?))
 (quantile-from-sorted-data
  (-> nonempty-sorted-vector-of-real? (real-in 0 1) real?))
 )