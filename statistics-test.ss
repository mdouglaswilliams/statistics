#lang scheme
;;; PLT Scheme Science Collection
;;; statistics-example.ss
;;; Copyright (c) 2004-2008 M. Douglas Williams
;;;
;;; This library is free software; you can redistribute it and/or 
;;; modify it under the terms of the GNU Lesser General Public 
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, 
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA.
;;;
;;; -------------------------------------------------------------------
;;;

(require (planet williams/science/random-distributions/gaussian)
         "statistics.ss"
         (planet williams/science/math))

(define (naive-sort! data)
  (let loop ()
    (let ((n (vector-length data))
          (sorted? #t))
      (do ((i 1 (+ i 1)))
          ((= i n) data)
        (when (< (vector-ref data i)
                 (vector-ref data (- i 1)))
          (let ((t (vector-ref data i)))
            (vector-set! data i (vector-ref data (- i 1)))
            (vector-set! data (- i 1) t)
            (set! sorted? #f))))
      (unless sorted?
        (loop)))))

(let ((data1 (make-vector 1000))
      (data2 (make-vector 1000))
      (w     (make-vector 1000)))
  (do ((i 0 (+ i 1)))
      ((= i 1000) (void))
    ;; Random data from unit gaussian
    (vector-set! data1 i (random-unit-gaussian))
    (vector-set! data2 i (random-unit-gaussian))
    ;; Cos^2 weighting
    (vector-set! w i
      (expt (cos (- (* 2.0 pi (/ i 1000.0)) pi)) 2)))
  (printf "Statistics Example~n")
  (printf "                                mean = ~a~n"
          (mean data1))
  (printf "                            variance = ~a~n"
          (variance data1))
  (printf "                  standard deviation = ~a~n"
          (standard-deviation data1))
  (printf "                   variance from 0.0 = ~a~n"
          (variance-with-fixed-mean data1 0.0))
  (printf "         standard deviation from 0.0 = ~a~n"
          (standard-deviation-with-fixed-mean data1 0.0))
  (printf "                  absolute deviation = ~a~n"
          (absolute-deviation data1))
  (printf "         absolute deviation from 0.0 = ~a~n"
          (absolute-deviation data1 0.0))
  (printf "                                skew = ~a~n"
          (skew data1))
  (printf "                            kurtosis = ~a~n"
          (kurtosis data1))
  (printf "               lag-1 autocorrelation = ~a~n"
          (lag-1-autocorrelation data1))
  (printf "                          covariance = ~a~n"
          (covariance data1 data2))
  (printf "                       weighted mean = ~a~n"
          (weighted-mean w data1))
  (printf "                   weighted variance = ~a~n"
          (weighted-variance w data1))
  (printf "         weighted standard deviation = ~a~n"
          (weighted-standard-deviation w data1))
  (printf "          weighted variance from 0.0 = ~a~n" 
          (weighted-variance-with-fixed-mean w data1 0.0))
  (printf "weighted standard deviation from 0.0 = ~a~n" 
          (weighted-standard-deviation-with-fixed-mean w data1 0.0))
  (printf "         weighted absolute deviation = ~a~n"
          (weighted-absolute-deviation w data1))
  (printf "weighted absolute deviation from 0.0 = ~a~n"
          (weighted-absolute-deviation w data1 0.0))
  (printf "                       weighted skew = ~a~n"
          (weighted-skew w data1))
  (printf "                   weighted kurtosis = ~a~n"
          (weighted-kurtosis w data1))
  (printf "                             maximum = ~a~n"
          (maximum data1))
  (printf "                             minimum = ~a~n"
          (minimum data1))
  (printf "              index of maximum value = ~a~n"
          (maximum-index data1))
  (printf "              index of minimum value = ~a~n"
          (minimum-index data1))
  (naive-sort! data1)
  (printf "                              median = ~a~n"
          (median-from-sorted-data data1))
  (printf "                        10% quantile = ~a~n"
          (quantile-from-sorted-data data1 .1))
  (printf "                        20% quantile = ~a~n"
          (quantile-from-sorted-data data1 .2))
  (printf "                        30% quantile = ~a~n"
          (quantile-from-sorted-data data1 .3))
  (printf "                        40% quantile = ~a~n"
          (quantile-from-sorted-data data1 .4))
  (printf "                        50% quantile = ~a~n"
          (quantile-from-sorted-data data1 .5))
  (printf "                        60% quantile = ~a~n"
          (quantile-from-sorted-data data1 .6))
  (printf "                        70% quantile = ~a~n"
          (quantile-from-sorted-data data1 .7))
  (printf "                        80% quantile = ~a~n"
          (quantile-from-sorted-data data1 .8))
  (printf "                        90% quantile = ~a~n"
          (quantile-from-sorted-data data1 .9))
)