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

(define-library (liii date-time internal database)
  (import (scheme base)
    (scheme process-context)
    (liii path)
    (liii date-time date)
    (liii date-time moment)
    (liii date-time time-delta)
    (liii date-time internal util)
    (liii date-time internal tz)
    (liii date-time internal tzif)
    (liii date-time internal dst)
    (liii date-time internal leap-seconds)
  ) ;import
  (export timezone? time-range time-range? time-range-start time-range-end
    time-range-offset time-range-dst? time-range-designation timezone-first
    timezone-utc-ranges timezone-local-ranges timezone-rule timezone-earliest
    timezone-latest find-offset/wall find-offset/utc utc-timezone
    utc-offset-timezone system-timezone tz-timezone tz-timezones
    reload-timezone-data!
  ) ;export
  (begin

    (define-record-type <time-range>
      (time-range start end offset dst? designation)
      time-range?
      (start time-range-start)
      (end time-range-end)
      (offset time-range-offset)
      (dst? time-range-dst?)
      (designation time-range-designation)
    ) ;define-record-type

    (define-record-type <timezone>
      (make-timezone first utc-ranges local-ranges rule)
      timezone?
      (first timezone-first)
      (utc-ranges timezone-utc-ranges)
      (local-ranges timezone-local-ranges)
      (rule timezone-rule)
    ) ;define-record-type

    (define *cached-timezones* (make-vector 101 '()))
    (define *timezone-names* #f)
    (define *system-timezone-cached* #f)

    (define unix-epoch-rd (date->rata-die (make-date 1970 1 1)))

    (define (seconds->time-delta s)
      (let-values (((m sec) (floor/ s 60)))
        (let-values (((h min) (floor/ m 60)))
          (time-delta+ (hours-delta h) (minutes-delta min) (seconds-delta sec))
        ) ;let-values
      ) ;let-values
    ) ;define

    (define (convert-tz-offset offset)
      (and offset
        (let ((sign (if (tz-offset-sign-negative? offset) 1 -1)))
          (time-delta+ (hours-delta (* sign (tz-offset-hour offset)))
            (minutes-delta (* sign (tz-offset-minute offset)))
            (seconds-delta (* sign (tz-offset-second offset)))
          ) ;time-delta+
        ) ;let
      ) ;and
    ) ;define

    (define (convert-footer-rule footer)
      (and footer
        (tzrule (tz-string-stdname footer)
          (convert-tz-offset (tz-string-stdoffset footer))
          (tz-string-change-to-dst footer)
          (tz-string-dstname footer)
          (convert-tz-offset (tz-string-dstoffset footer))
          (tz-string-change-to-std footer)
        ) ;tzrule
      ) ;and
    ) ;define

    (define (build-ranges first-trans transitions)
      (let* ((len (vector-length transitions)))
        (if (zero? len)
          (let ((off (if first-trans
                       (seconds->time-delta (tzif-transition-offset first-trans))
                       (seconds-delta 0)
                     ) ;if
                ) ;off
                (dst (if first-trans (tzif-transition-dst? first-trans) #f))
                (des (if first-trans (tzif-transition-designation first-trans) "UTC"))
               ) ;
            (let ((r (vector (time-range #f #f off dst des))))
              (values r r)
            ) ;let
          ) ;let
          (let* ((utc-v (make-vector (+ len 1)))
                 (loc-v (make-vector (+ len 1)))
                 (first-off (seconds->time-delta (tzif-transition-offset first-trans)))
                 (first-dst (tzif-transition-dst? first-trans))
                 (first-des (tzif-transition-designation first-trans))
                 (first-t (tzif-transition-time (vector-ref transitions 0)))
                ) ;
            ;; First range
            (vector-set! utc-v 0 (time-range #f first-t first-off first-dst first-des))
            (let ((loc-end (+ first-t (tzif-transition-offset first-trans))))
              (vector-set! loc-v 0 (time-range #f loc-end first-off first-dst first-des))
            ) ;let

            ;; Intermediate ranges
            (do ((i 0 (+ i 1)))
              ((= i (- len 1)))
              (let* ((curr (vector-ref transitions i))
                     (nxt (vector-ref transitions (+ i 1)))
                     (t1 (tzif-transition-time curr))
                     (t2 (tzif-transition-time nxt))
                     (off-sec (tzif-transition-offset curr))
                     (off (seconds->time-delta off-sec))
                     (dst (tzif-transition-dst? curr))
                     (des (tzif-transition-designation curr))
                    ) ;
                (vector-set! utc-v (+ i 1) (time-range t1 t2 off dst des))
                (vector-set! loc-v
                  (+ i 1)
                  (time-range (+ t1 off-sec) (+ t2 off-sec) off dst des)
                ) ;vector-set!
              ) ;let*
            ) ;do

            ;; Last range
            (let* ((last-idx (- len 1))
                   (curr (vector-ref transitions last-idx))
                   (t1 (tzif-transition-time curr))
                   (off-sec (tzif-transition-offset curr))
                   (off (seconds->time-delta off-sec))
                   (dst (tzif-transition-dst? curr))
                   (des (tzif-transition-designation curr))
                  ) ;
              (vector-set! utc-v (+ last-idx 1) (time-range t1 #f off dst des))
              (vector-set! loc-v (+ last-idx 1) (time-range (+ t1 off-sec) #f off dst des))
            ) ;let*

            (values utc-v loc-v)
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (load-timezone-from-file filepath)
      (let-values (((first transitions footer) (read-tzif-file filepath)))
        (let-values (((utc-ranges loc-ranges) (build-ranges first transitions)))
          (let ((rule (convert-footer-rule footer)))
            (make-timezone first utc-ranges loc-ranges rule)
          ) ;let
        ) ;let-values
      ) ;let-values
    ) ;define

    (define (hash-str str)
      (let ((len (string-length str)))
        (let loop
          ((i 0) (h 0))
          (if (= i len)
            (modulo h 101)
            (loop (+ i 1) (+ (* h 31) (char->integer (string-ref str i))))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (get-cached-timezone name)
      (let* ((idx (hash-str name)) (bucket (vector-ref *cached-timezones* idx)))
        (cond ((assoc name bucket) => cdr)
              (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    (define (cache-timezone! name tz)
      (let* ((idx (hash-str name)) (bucket (vector-ref *cached-timezones* idx)))
        (vector-set! *cached-timezones* idx (cons (cons name tz) bucket))
        tz
      ) ;let*
    ) ;define

    (define (tz-timezone name)
      (unless (string? name)
        (error 'type-error "tz-timezone: expected a string timezone name" name)
      ) ;unless
      (let ((cached (get-cached-timezone name)))
        (if cached
          cached
          (let ((path (string-append "/usr/share/zoneinfo/" name)))
            (if (file-exists? path)
              (let ((tz (load-timezone-from-file path)))
                (cache-timezone! name tz)
              ) ;let
              (error 'type-error "tz-timezone: unknown timezone" name)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (discover-zoneinfo-names)
      (let ((base "/usr/share/zoneinfo"))
        (if (not (file-exists? base))
          '("UTC")
          (let loop
            ((subdirs '("")) (acc '()))
            (if (null? subdirs)
              acc
              (let* ((curr (car subdirs))
                     (dir-path (if (string=? curr "") base (string-append base "/" curr)))
                    ) ;
                (if (not (path-dir? (path dir-path)))
                  (loop (cdr subdirs) acc)
                  (let* ((entries (path-list (path dir-path)))
                         (next-subdirs (cdr subdirs))
                         (new-names acc)
                        ) ;
                    (for-each
                      (lambda (ent)
                        (let* ((name (path->string ent))
                               (rel (if (string=? curr "") name (string-append curr "/" name)))
                               (full (string-append base "/" rel))
                              ) ;
                          (cond ((member name '("." ".." "posix" "right" "Etc")) #f)
                                ((path-dir? (path full)) (set! next-subdirs (cons rel next-subdirs)))
                                ((and (not (string=? (path-suffix ent) ".tab"))
                                   (not (string=? (path-suffix ent) ".zi"))
                                   (not (string=? (path-suffix ent) ".list"))
                                 ) ;and
                                 (set! new-names (cons rel new-names))
                                ) ;
                          ) ;cond
                        ) ;let*
                      ) ;lambda
                      entries
                    ) ;for-each
                    (loop next-subdirs new-names)
                  ) ;let*
                ) ;if
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (tz-timezones)
      (unless *timezone-names*
        (set! *timezone-names* (discover-zoneinfo-names))
      ) ;unless
      *timezone-names*
    ) ;define

    (define (reload-timezone-data!)
      (set! *cached-timezones* (make-vector 101 '()))
      (set! *timezone-names* #f)
      (set! *system-timezone-cached* #f)
    ) ;define

    (define (utc-timezone)
      (let ((r (vector (time-range #f #f (seconds-delta 0) #f "UTC"))))
        (make-timezone #f r r #f)
      ) ;let
    ) ;define

    (define (utc-offset-timezone offset)
      (unless (time-delta? offset)
        (error 'type-error "utc-offset-timezone: expected a time-delta" offset)
      ) ;unless
      (unless (and (zero? (time-delta-years offset))
                (zero? (time-delta-months offset))
                (zero? (time-delta-weeks offset))
                (zero? (time-delta-days offset))
              ) ;and
        (error 'type-error
          "utc-offset-timezone: offset cannot have year/month/week/day"
          offset
        ) ;error
      ) ;unless
      (let* ((h (time-delta-hours offset))
             (m (time-delta-minutes offset))
             (s (time-delta-seconds offset))
             (neg? (or (negative? h) (negative? m) (negative? s)))
             (sign-str (if neg? "-" "+"))
             (desig
               (string-append "UTC"
                 sign-str
                 (left-pad (number->string (abs h)) #\0 2)
                 ":"
                 (left-pad (number->string (abs m)) #\0 2)
               ) ;string-append
             ) ;desig
             (r (vector (time-range #f #f offset #f desig)))
            ) ;
        (make-timezone #f r r #f)
      ) ;let*
    ) ;define

    (define (system-timezone)
      (unless *system-timezone-cached*
        (let ((env-tz (get-environment-variable "TZ")))
          (cond
           ((and env-tz (not (string=? env-tz "")))
            (set! *system-timezone-cached*
              (if (char=? (string-ref env-tz 0) #\:)
                (tz-timezone (substring env-tz 1))
                (tz-timezone env-tz)
              ) ;if
            ) ;set!
           ) ;
           ((file-exists? "/etc/localtime")
            (set! *system-timezone-cached* (load-timezone-from-file "/etc/localtime"))
           ) ;
           (else (set! *system-timezone-cached* (utc-timezone)))
          ) ;cond
        ) ;let
      ) ;unless
      *system-timezone-cached*
    ) ;define

    (define (search-ranges ranges time)
      (let loop
        ((l 0) (r (- (vector-length ranges) 1)))
        (if (< r l)
          #f
          (let* ((mid (+ l (floor-quotient (- r l) 2)))
                 (entry (vector-ref ranges mid))
                 (st (time-range-start entry))
                 (en (time-range-end entry))
                ) ;
            (cond
             ((and (or (not st) (<= st time)) (or (not en) (< time en))) mid)
             ((and st (< time st)) (loop l (- mid 1)))
             (else (loop (+ mid 1) r))
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (find-offset/utc tz unix-time)
      (let* ((ranges (timezone-utc-ranges tz)) (i (search-ranges ranges unix-time)))
        (cond ((not i) (vector-ref ranges 0))
              ((and (= i (- (vector-length ranges) 1)) (timezone-rule tz))
               (let* ((rule (timezone-rule tz))
                      (std-offset (tzrule-std-offset rule))
                      (std-designation (tzrule-std-name rule))
                     ) ;
                 (if (not (tzrule-dst-transition rule))
                   (time-range #f #f std-offset #f std-designation)
                   (let* ((dst-offset (tzrule-dst-offset rule))
                          (std-transition (tzrule-std-transition rule))
                          (dst-transition (tzrule-dst-transition rule))
                          (dst-designation (tzrule-dst-name rule))
                          (dst->std (lambda (year . opt-offset) (transition-unix dst-offset year std-transition))
                          ) ;dst->std
                          (std->dst (lambda (year . opt-offset) (transition-unix std-offset year dst-transition))
                          ) ;std->dst
                         ) ;
                     (let-values (((year subindex)
                                   (find-an-offset-rule unix-time dst->std std->dst dst-offset std-offset)
                                  ) ;
                                 ) ;
                       (let ((v (offset-rule time-range unix-time year subindex
                                  dst->std std->dst dst-offset std-offset
                                  dst-designation std-designation
                                ) ;offset-rule
                             ) ;v
                            ) ;
                         (if (> (vector-length v) 0)
                           (vector-ref v 0)
                           (time-range #f #f std-offset #f std-designation)
                         ) ;if
                       ) ;let
                     ) ;let-values
                   ) ;let*
                 ) ;if
               ) ;let*
              ) ;
              (else (vector-ref ranges i))
        ) ;cond
      ) ;let*
    ) ;define

    (define (find-offset/wall tz local-time)
      (let* ((ranges (timezone-local-ranges tz))
             (i (search-ranges ranges local-time))
             (is-in*
               (lambda (val)
                 (let ((st (time-range-start val)) (en (time-range-end val)))
                   (if (and (or (not st) (<= st local-time)) (or (not en) (< local-time en)))
                     (vector val)
                     #()
                   ) ;if
                 ) ;let
               ) ;lambda
             ) ;is-in*
             (is-in
               (lambda (idx)
                 (if (or (< idx 0) (>= idx (vector-length ranges)))
                   #()
                   (is-in* (vector-ref ranges idx))
                 ) ;if
               ) ;lambda
             ) ;is-in
            ) ;
        (cond
         ((and i (= i (- (vector-length ranges) 1)) (timezone-rule tz))
          (let* ((rule (timezone-rule tz))
                 (std-offset (tzrule-std-offset rule))
                 (std-designation (tzrule-std-name rule))
                ) ;
            (if (not (tzrule-dst-transition rule))
              (vector (time-range #f #f std-offset #f std-designation))
              (let* ((dst-offset (tzrule-dst-offset rule))
                     (std-transition (tzrule-std-transition rule))
                     (dst-transition (tzrule-dst-transition rule))
                     (dst-designation (tzrule-dst-name rule))
                     (dst->std (lambda (year offset)
                                 (add-offset (transition-unix dst-offset year std-transition) offset)
                               ) ;lambda
                     ) ;dst->std
                     (std->dst (lambda (year offset)
                                 (add-offset (transition-unix std-offset year dst-transition) offset)
                               ) ;lambda
                     ) ;std->dst
                    ) ;
                (let-values (((year subindex)
                              (find-an-offset-rule local-time dst->std std->dst dst-offset std-offset)
                             ) ;
                            ) ;
                  (let ((is-in-rule (lambda (subidx)
                                      (offset-rule time-range local-time year
                                        subidx dst->std std->dst dst-offset
                                        std-offset dst-designation
                                        std-designation
                                      ) ;offset-rule
                                    ) ;lambda
                        ) ;is-in-rule
                       ) ;
                    (vector-append (is-in-rule (- subindex 1))
                      (is-in-rule subindex)
                      (is-in-rule (+ subindex 1))
                    ) ;vector-append
                  ) ;let
                ) ;let-values
              ) ;let*
            ) ;if
          ) ;let*
         ) ;
         ((not i) #())
         (else (vector-append (is-in (- i 1)) (is-in i) (is-in (+ i 1))))
        ) ;cond
      ) ;let*
    ) ;define

    (define (timezone-earliest tz)
      (let* ((first (vector-ref (timezone-utc-ranges tz) 0)) (utc (time-range-end first)))
        (let-values (((_ tai-utc) (leapsecond-info/utc (or utc 0))))
          (let-values (((days sec) (floor/ (+ (or utc 0) tai-utc) 86400)))
            (make-moment (rata-die->date (+ days unix-epoch-rd)) sec)
          ) ;let-values
        ) ;let-values
      ) ;let*
    ) ;define

    (define (timezone-latest tz)
      (let* ((ranges (timezone-utc-ranges tz))
             (last (vector-ref ranges (- (vector-length ranges) 1)))
             (utc (time-range-start last))
            ) ;
        (let-values (((_ tai-utc) (leapsecond-info/utc (or utc 0))))
          (let-values (((days sec) (floor/ (+ (or utc 0) tai-utc) 86400)))
            (make-moment (rata-die->date (+ days unix-epoch-rd)) sec)
          ) ;let-values
        ) ;let-values
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
