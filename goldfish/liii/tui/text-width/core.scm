;;
;; Copyright (C) 2024 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii tui text-width core)
  (export %match-ansi-csi %codepoint-width %is-sgr-reset? strip-ansi
    display-width
  ) ;export
  (import (scheme base) (liii base) (liii error) (liii unicode))
  (begin

    (define (%match-ansi-csi bv i len)
      (if
        (and (< (+ i 1) len)
          (= (bytevector-u8-ref bv i) 27)
          (= (bytevector-u8-ref bv (+ i 1)) 91)
        ) ;and
        (let loop
          ((j (+ i 2)))
          (if (>= j len)
            #f
            (let ((b (bytevector-u8-ref bv j)))
              (cond ((and (>= b 32) (<= b 63)) (loop (+ j 1)))
                    ((and (>= b 64) (<= b 126)) (+ j 1))
                    (else #f)
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
        #f
      ) ;if
    ) ;define

    (define (%codepoint-width cp)
      (cond
       ((or (< cp 32) (and (>= cp 127) (< cp 160))) 0)
       ((or (= cp 173)
          (= cp 8203)
          (= cp 8204)
          (= cp 8205)
          (= cp 65279)
          (and (>= cp 768) (<= cp 879))
          (and (>= cp 7616) (<= cp 7679))
          (and (>= cp 8400) (<= cp 8447))
          (and (>= cp 65056) (<= cp 65071))
        ) ;or
        0
       ) ;
       ((or (and (>= cp 4352) (<= cp 4447))
          (and (>= cp 11904) (<= cp 42191))
          (and (>= cp 44032) (<= cp 55203))
          (and (>= cp 63744) (<= cp 64255))
          (and (>= cp 65040) (<= cp 65049))
          (and (>= cp 65072) (<= cp 65135))
          (and (>= cp 65280) (<= cp 65376))
          (and (>= cp 65504) (<= cp 65510))
          (and (>= cp 131072) (<= cp 196605))
          (and (>= cp 196608) (<= cp 262141))
          (and (>= cp 9728) (<= cp 10175))
          (and (>= cp 127744) (<= cp 129791))
        ) ;or
        2
       ) ;
       (else 1)
      ) ;cond
    ) ;define

    (define (%is-sgr-reset? s)
      (or (string=? s "\x1b;[0m") (string=? s "\x1b;[m"))
    ) ;define

    (define (strip-ansi str)
      (unless (string? str)
        (type-error "strip-ansi: str must be a string")
      ) ;unless
      (if (string=? str "")
        ""
        (let* ((bv (string->utf8 str)) (len (bytevector-length bv)))
          (let loop
            ((i 0) (acc '()))
            (if (>= i len)
              (apply string-append (reverse acc))
              (let ((ansi-end (%match-ansi-csi bv i len)))
                (if ansi-end
                  (loop ansi-end acc)
                  (let* ((next (bytevector-advance-utf8 bv i len))
                         (step (if (= next i) (+ i 1) next))
                         (chunk (utf8->string (bytevector-copy bv i step)))
                        ) ;
                    (loop step (cons chunk acc))
                  ) ;let*
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

    (define (display-width str)
      (unless (string? str)
        (type-error "display-width: str must be a string")
      ) ;unless
      (if (string=? str "")
        0
        (let* ((bv (string->utf8 str)) (len (bytevector-length bv)))
          (let loop
            ((i 0) (w 0))
            (if (>= i len)
              w
              (let ((ansi-end (%match-ansi-csi bv i len)))
                (if ansi-end
                  (loop ansi-end w)
                  (let* ((next (bytevector-advance-utf8 bv i len))
                         (step (if (= next i) (+ i 1) next))
                         (cp (utf8->codepoint-at bv i))
                         (cw (%codepoint-width cp))
                        ) ;
                    (loop step (+ w cw))
                  ) ;let*
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
