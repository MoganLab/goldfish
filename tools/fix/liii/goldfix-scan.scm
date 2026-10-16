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

(define-library (liii goldfix-scan)
  (export whitespace-char? delimiter-char? string-prefix-at? find-string-end
    find-bar-symbol-end find-block-comment-end find-raw-string-end find-char-end
    find-line-comment-end find-other-end
  ) ;export
  (import (liii base) (liii string))

  (begin
    (define (whitespace-char? c)
      (or (char=? c #\space)
        (char=? c #\tab)
        (char=? c #\newline)
        (char=? c #\return)
      ) ;or
    ) ;define

    (define (delimiter-char? c)
      (or (whitespace-char? c)
        (char=? c #\()
        (char=? c #\))
        (char=? c #\;)
        (char=? c #\")
      ) ;or
    ) ;define
    (define (string-prefix-at? str prefix pos)
      (let ((len (string-length str)) (prefix-len (string-length prefix)))
        (and (<= (+ pos prefix-len) len)
          (string=? (substring str pos (+ pos prefix-len)) prefix)
        ) ;and
      ) ;let
    ) ;define
    (define (find-string-end source start)
      (let ((len (string-length source)))
        (let loop
          ((i (+ start 1)) (escaped #f))
          (cond ((>= i len) len)
                (escaped (loop (+ i 1) #f))
                ((char=? (string-ref source i) #\\) (loop (+ i 1) #t))
                ((char=? (string-ref source i) #\") (+ i 1))
                (else (loop (+ i 1) #f))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (find-bar-symbol-end source start)
      (let ((len (string-length source)))
        (let loop
          ((i (+ start 1)) (escaped #f))
          (cond ((>= i len) len)
                (escaped (loop (+ i 1) #f))
                ((char=? (string-ref source i) #\\) (loop (+ i 1) #t))
                ((char=? (string-ref source i) #\|) (+ i 1))
                (else (loop (+ i 1) #f))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (find-block-comment-end source start)
      (let ((len (string-length source)))
        (let loop
          ((i (+ start 2)) (level 1))
          (cond ((>= i len) len)
                ((and (< (+ i 1) len)
                   (char=? (string-ref source i) #\#)
                   (char=? (string-ref source (+ i 1)) #\|)
                 ) ;and
                 (loop (+ i 2) (+ level 1))
                ) ;
                ((and (< (+ i 1) len)
                   (char=? (string-ref source i) #\|)
                   (char=? (string-ref source (+ i 1)) #\#)
                 ) ;and
                 (if (= level 1) (+ i 2) (loop (+ i 2) (- level 1)))
                ) ;
                (else (loop (+ i 1) level))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (find-raw-string-end source start)
      (let* ((len (string-length source))
             (delimiter-end (string-index source #\" (+ start 2)))
            ) ;
        (if delimiter-end
          (let* ((delimiter (substring source (+ start 2) delimiter-end))
                 (delimiter-len (string-length delimiter))
                 (content-start (+ delimiter-end 1))
                ) ;
            (if (= delimiter-len 0)
              (let loop
                ((i content-start))
                (cond ((>= i len) len)
                      ((and (< (+ i 1) len)
                         (char=? (string-ref source i) #\")
                         (char=? (string-ref source (+ i 1)) #\")
                       ) ;and
                       (+ i 2)
                      ) ;
                      (else (loop (+ i 1)))
                ) ;cond
              ) ;let
              (let loop
                ((i content-start))
                (cond ((>= i len) len)
                      ((and (<= (+ i delimiter-len) len)
                         (string-prefix-at? source delimiter i)
                         (< (+ i delimiter-len) len)
                         (char=? (string-ref source (+ i delimiter-len)) #\")
                       ) ;and
                       (+ i delimiter-len 1)
                      ) ;
                      (else (loop (+ i 1)))
                ) ;cond
              ) ;let
            ) ;if
          ) ;let*
          len
        ) ;if
      ) ;let*
    ) ;define
    (define (find-char-end source start)
      (let ((len (string-length source)))
        (if (>= (+ start 2) len)
          len
          (let ((c (string-ref source (+ start 2))))
            (if (delimiter-char? c)
              (+ start 3)
              (let loop
                ((i (+ start 3)))
                (if (or (>= i len) (delimiter-char? (string-ref source i))) i (loop (+ i 1)))
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (find-line-comment-end source start)
      (let ((len (string-length source)))
        (let loop
          ((i start))
          (cond ((>= i len) len)
                ((char=? (string-ref source i) #\newline) i)
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (find-other-end source start)
      (let ((len (string-length source)))
        (let loop
          ((i start))
          (cond ((>= i len) len)
                ((delimiter-char? (string-ref source i)) i)
                ((and (< (+ i 1) len)
                   (char=? (string-ref source i) #\#)
                   (char=? (string-ref source (+ i 1)) #\|)
                 ) ;and
                 i
                ) ;
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
