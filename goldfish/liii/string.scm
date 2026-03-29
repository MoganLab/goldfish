;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii string)
  (export
    ; S7 built-in
    string? string-ref string-length
    ; from (scheme base)
    string-copy string-for-each string-map
    ; from (srfi srfi-13)
    string-null? string-join
    string-every string-any
    string-take string-take-right string-drop string-drop-right
    string-pad string-pad-right
    string-trim string-trim-right string-trim-both
    string-index string-index-right string-skip string-skip-right
    string-contains string-count
    string-upcase string-downcase
    string-fold string-fold-right string-for-each-index
    string-reverse
    string-tokenize
    ; Liii extras
    string-starts? string-contains? string-ends?
    string-split string-replace
    string-remove-prefix string-remove-suffix
  ) ;export
  (import (except (srfi srfi-13) string-replace)
          (liii base)
          (liii error)
  ) ;import
  (begin

    (define (string-starts? str prefix)
      (if (and (string? str) (string? prefix))
          (string-prefix? prefix str)
          (type-error "string-starts? parameter is not a string")
      ) ;if
    ) ;define

    (define string-contains?
      (typed-lambda ((str string?) (sub-str string?))
        (string-contains str sub-str)
      ) ;typed-lambda
    ) ;define

    (define (string-split str sep)
      (define (split-characters input)
        (let ((input-len (utf8-string-length input)))
          (let loop ((i 0)
                     (parts '()))
            (if (= i input-len)
                (reverse parts)
                (loop (+ i 1)
                      (cons (u8-substring input i (+ i 1))
                            parts
                      ) ;cons
                ) ;loop
            ) ;if
          ) ;let loop
        ) ;let
      ) ;define

      (when (not (string? str))
        (type-error "string-split: first parameter must be string")
      ) ;when

      (let* ((sep-str (cond ((string? sep) sep)
                            ((char? sep) (string sep))
                            (else (type-error "string-split: second parameter must be string or char"))
                      ) ;cond
             )
             (str-len (string-length str))
             (sep-len (string-length sep-str)))
        (if (zero? sep-len)
            (split-characters str)
            (let loop ((search-start 0)
                       (parts '()))
              (let ((next-pos (string-position sep-str str search-start)))
                (if next-pos
                    (loop (+ next-pos sep-len)
                          (cons (substring str search-start next-pos)
                                parts
                          ) ;cons
                    ) ;loop
                    (reverse
                      (cons (substring str search-start str-len)
                            parts
                      ) ;cons
                    ) ;reverse
                ) ;if
              ) ;let
            ) ;let loop
        ) ;if
      ) ;let*
    ) ;define

    (define string-replace
      (typed-lambda ((str string?) (old string?) (new string?))
        (let ((str-len (string-length str))
              (old-len (string-length old)))
          (if (zero? old-len)
              ; Python 兼容行为: 空 pattern 时在每个字符之间插入 new
              (if (zero? str-len)
                  new  ; 空字符串 + 空 pattern = new
                  (let loop ((i 0)
                             (acc (list new)))
                    (if (= i str-len)
                        (apply string-append (reverse acc))
                        (loop (+ i 1)
                              (cons new
                                    (cons (substring str i (+ i 1))
                                          acc))))))
              (let loop ((search-start 0)
                         (parts '()))
                (let ((next-pos (string-position old str search-start)))
                  (if next-pos
                      (loop (+ next-pos old-len)
                            (cons new
                                  (cons (substring str search-start next-pos)
                                        parts
                                  ) ;cons
                            ) ;cons
                      ) ;loop
                      (if (null? parts)
                          (string-copy str)
                          (apply string-append
                                 (reverse
                                   (cons (substring str search-start str-len)
                                         parts
                                   ) ;cons
                                 ) ;reverse
                          ) ;apply
                      ) ;if
                  ) ;if
                ) ;let
              ) ;let loop
          ) ;if
        ) ;let
      ) ;typed-lambda
    ) ;define

    (define (string-ends? str suffix)
      (if (and (string? str) (string? suffix))
          (string-suffix? suffix str)
          (type-error "string-ends? parameter is not a string")
      ) ;if
    ) ;define

    (define string-remove-prefix
      (typed-lambda ((str string?) (prefix string?))
        (if (string-prefix? prefix str)
            (substring str (string-length prefix))
            str
        ) ;if
      ) ;typed-lambda
    ) ;define

    (define string-remove-suffix
      (typed-lambda ((str string?) (suffix string?))
        (if (string-suffix? suffix str)
            (substring str 0 (- (string-length str) (string-length suffix)))
            (string-copy str)
        ) ;if
      ) ;typed-lambda
    ) ;define

  ) ;begin
) ;define-library
