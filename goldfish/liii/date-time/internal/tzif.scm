;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;; Copyright (C) 2026 Peter McGoron
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;

(define-library (liii date-time internal tzif)
  (import (scheme base) (liii path) (liii date-time internal tz))
  (export tzif-transition tzif-transition? tzif-transition-time
    tzif-transition-offset tzif-transition-dst? tzif-transition-designation
    tzif-transition-standard? tzif-transition-universal-time? read-tzif-file
  ) ;export
  (begin

    (define-record-type <tzif-transition>
      (tzif-transition time offset dst? designation standard? universal-time?)
      tzif-transition?
      (time tzif-transition-time)
      (offset tzif-transition-offset)
      (dst? tzif-transition-dst?)
      (designation tzif-transition-designation)
      (standard? tzif-transition-standard?)
      (universal-time? tzif-transition-universal-time?)
    ) ;define-record-type

    (define-record-type <local-time-type>
      (local-time-type offset dst? desigidx)
      local-time-type?
      (offset local-time-type-offset)
      (dst? local-time-type-dst?)
      (desigidx local-time-type-desigidx)
    ) ;define-record-type

    (define (bv-u32 bv pos)
      (+ (* (bytevector-u8-ref bv pos) 16777216)
        (* (bytevector-u8-ref bv (+ pos 1)) 65536)
        (* (bytevector-u8-ref bv (+ pos 2)) 256)
        (bytevector-u8-ref bv (+ pos 3))
      ) ;+
    ) ;define

    (define (bv-s32 bv pos)
      (let ((v (bv-u32 bv pos)))
        (if (>= v 2147483648) (- v 4294967296) v)
      ) ;let
    ) ;define

    (define (bv-s64 bv pos)
      (let ((hi (bv-s32 bv pos)) (lo (bv-u32 bv (+ pos 4))))
        (+ (* hi 4294967296) lo)
      ) ;let
    ) ;define

    (define (read-designation-str bv start max-end)
      (let loop
        ((i start) (chars '()))
        (if (or (>= i max-end) (zero? (bytevector-u8-ref bv i)))
          (list->string (reverse chars))
          (loop (+ i 1) (cons (integer->char (bytevector-u8-ref bv i)) chars))
        ) ;if
      ) ;let
    ) ;define

    (define (parse-block bv offset-ref is-64bit start-pos)
      (let* ((version-byte (bytevector-u8-ref bv (+ start-pos 4)))
             (version (case version-byte ((0) 1) ((50) 2) ((51) 3) ((52) 4) (else 4)))
             (header-pos (+ start-pos 20))
             (isutcnt (bv-u32 bv header-pos))
             (isstdcnt (bv-u32 bv (+ header-pos 4)))
             (leapcnt (bv-u32 bv (+ header-pos 8)))
             (timecnt (bv-u32 bv (+ header-pos 12)))
             (typecnt (bv-u32 bv (+ header-pos 16)))
             (charcnt (bv-u32 bv (+ header-pos 20)))
             (data-pos (+ header-pos 24))
             (time-size (if is-64bit 8 4))
             (time-reader (if is-64bit bv-s64 bv-s32))
            ) ;

        (let* ((times-pos data-pos)
               (types-pos (+ times-pos (* timecnt time-size)))
               (types-rec-pos (+ types-pos timecnt))
               (desigs-pos (+ types-rec-pos (* typecnt 6)))
               (leap-pos (+ desigs-pos charcnt))
               (leap-size (if is-64bit 12 8))
               (std-pos (+ leap-pos (* leapcnt leap-size)))
               (ut-pos (+ std-pos isstdcnt))
               (next-block-pos (+ ut-pos isutcnt))
              ) ;

          (let ((types-vec (make-vector typecnt)))
            (do ((i 0 (+ i 1)))
              ((= i typecnt))
              (let* ((r-pos (+ types-rec-pos (* i 6)))
                     (off (bv-s32 bv r-pos))
                     (dst
                       (not (zero? (bytevector-u8-ref bv (+ r-pos 4))))
                     ) ;dst
                     (d-idx (bytevector-u8-ref bv (+ r-pos 5)))
                    ) ;
                (vector-set! types-vec i (local-time-type off dst d-idx))
              ) ;let*
            ) ;do

            (define (derive-transition t type-idx)
              (let* ((ltype (vector-ref types-vec (min type-idx (- typecnt 1))))
                     (off (local-time-type-offset ltype))
                     (dst? (local-time-type-dst? ltype))
                     (didx (local-time-type-desigidx ltype))
                     (desig (read-designation-str bv (+ desigs-pos didx) (+ desigs-pos charcnt)))
                     (std?
                       (if (< type-idx isstdcnt) (= (bytevector-u8-ref bv (+ std-pos type-idx)) 1) #f)
                     ) ;std?
                     (ut?
                       (if (< type-idx isutcnt) (= (bytevector-u8-ref bv (+ ut-pos type-idx)) 1) #f)
                     ) ;ut?
                    ) ;
                (tzif-transition t off dst? desig std? ut?)
              ) ;let*
            ) ;define

            (let* ((first (if (> typecnt 0) (derive-transition #f 0) #f))
                   (transitions (make-vector timecnt))
                  ) ;
              (do ((i 0 (+ i 1)))
                ((= i timecnt))
                (let* ((t (time-reader bv (+ times-pos (* i time-size))))
                       (t-idx (bytevector-u8-ref bv (+ types-pos i)))
                      ) ;
                  (vector-set! transitions i (derive-transition t t-idx))
                ) ;let*
              ) ;do
              (values version first transitions next-block-pos)
            ) ;let*
          ) ;let
        ) ;let*
      ) ;let*
    ) ;define

    (define (read-tzif-file path-str)
      (let ((bv (path-read-bytes (path path-str))))
        (unless
          (and (>= (bytevector-length bv) 4)
            (char=? (integer->char (bytevector-u8-ref bv 0)) #\T)
            (char=? (integer->char (bytevector-u8-ref bv 1)) #\Z)
            (char=? (integer->char (bytevector-u8-ref bv 2)) #\i)
            (char=? (integer->char (bytevector-u8-ref bv 3)) #\f)
          ) ;and
          (error 'type-error "read-tzif-file: not a valid TZif file" path-str)
        ) ;unless

        (let-values (((v1 first1 trans1 next-pos) (parse-block bv bv-s32 #f 0)))
          (if (= v1 1)
            (values first1 trans1 #f)
            ;; Version 2+ : read second block
            (let-values (((v2 first2 trans2 footer-start) (parse-block bv bv-s64 #t next-pos)))
              ;; parse footer if present
              (let ((footer
                      (if (and (< footer-start (bytevector-length bv))
                            (= (bytevector-u8-ref bv footer-start) 10)
                          ) ;and
                        (let loop
                          ((i (+ footer-start 1)) (acc '()))
                          (if (>= i (bytevector-length bv))
                            #f
                            (let ((b (bytevector-u8-ref bv i)))
                              (if (= b 10)
                                (let ((str (list->string (reverse acc))))
                                  (if (string=? str "") #f (parse-tz str))
                                ) ;let
                                (loop (+ i 1) (cons (integer->char b) acc))
                              ) ;if
                            ) ;let
                          ) ;if
                        ) ;let
                        #f
                      ) ;if
                    ) ;footer
                   ) ;
                (values first2 trans2 footer)
              ) ;let
            ) ;let-values
          ) ;if
        ) ;let-values
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
