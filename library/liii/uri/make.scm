;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; ; (liii uri make) - URI 构造器子库
;; ; 函数文档见 bin/gf doc liii/uri 与 library/tests/liii/uri/make/README.md

(define-library (liii uri make)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii error)
    (liii base)
    (liii uri internal record)
    (liii uri internal parse)
    (liii uri record)
  ) ;import

  ;; ; ---------- 导出接口 ----------
  ;; 构造 URI
  (export make-uri string->uri)
  (export uri-build)

  (begin

    ;; ; ---------- 解析辅助 ----------
    (define (split-scheme str)
      (let ((colon-pos (string-index str #\:)))
        (if colon-pos
          (cons (substring str 0 colon-pos)
            (substring str (+ colon-pos 1) (string-length str))
          ) ;cons
          (cons #f str)
        ) ;if
      ) ;let
    ) ;define

    (define (split-authority str)
      (let loop
        ((i 0) (slash-pos #f) (question-pos #f) (hash-pos #f))
        (if (>= i (string-length str))
          (let ((netloc (if slash-pos (substring str 0 slash-pos) str))
                (path
                  (if slash-pos
                    (substring str slash-pos (or question-pos hash-pos (string-length str)))
                    ""
                  ) ;if
                ) ;path
                (query
                  (if question-pos
                    (substring str (+ question-pos 1) (or hash-pos (string-length str)))
                    ""
                  ) ;if
                ) ;query
                (fragment (if hash-pos (substring str (+ hash-pos 1) (string-length str)) #f))
               ) ;
            (list netloc path query fragment)
          ) ;let
          (let ((c (string-ref str i)))
            (cond
             ((char=? c #\/)
              (if slash-pos
                (loop (+ i 1) slash-pos question-pos hash-pos)
                (loop (+ i 1) i question-pos hash-pos)
              ) ;if
             ) ;
             ((char=? c #\?) (loop (+ i 1) (or slash-pos i) i hash-pos))
             ((char=? c #\#) (loop (+ i 1) (or slash-pos i) question-pos i))
             (else (loop (+ i 1) slash-pos question-pos hash-pos))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; ; ---------- 构造函数 ----------
    (define (make-uri str)
      (if (not (string? str))
        (type-error "make-uri: expected string")
        (if
          (and (string-index str #\@)
            (not (string-starts? str "http://"))
            (not (string-starts? str "https://"))
            (not (string-starts? str "ssh://"))
            (let ((colon-pos (string-index str #\:)))
              (and colon-pos (> colon-pos (string-index str #\@)))
            ) ;let
          ) ;and
          (let* ((at-pos (string-index str #\@))
                 (colon-pos (string-index str #\:))
                 (user (substring str 0 at-pos))
                 (host (substring str (+ at-pos 1) colon-pos))
                 (path (substring str colon-pos (string-length str)))
                ) ;
            (%make-uri-record "ssh" (build-netloc user #f host #f) path '() #f)
          ) ;let*
          (let* ((scheme+rest (split-scheme str))
                 (scheme (car scheme+rest))
                 (rest (cdr scheme+rest))
                 (authority+path+query+frag
                   (if (string-starts? rest "//")
                     (split-authority (substring rest 2 (string-length rest)))
                     (list "" rest "" #f)
                   ) ;if
                 ) ;authority+path+query+frag
                 (netloc (list-ref authority+path+query+frag 0))
                 (path (list-ref authority+path+query+frag 1))
                 (query-str (list-ref authority+path+query+frag 2))
                 (fragment (list-ref authority+path+query+frag 3))
                ) ;
            (%make-uri-record scheme netloc path (query-string->alist query-str) fragment)
          ) ;let*
        ) ;if
      ) ;if
    ) ;define

    (define* (uri-build (scheme #f)
               (user #f)
               (password #f)
               (host #f)
               (port #f)
               (path #f)
               (query '())
               (fragment #f)
             ) ;uri-build
      (%make-uri-record scheme
        (build-netloc user password host port)
        (or path "")
        query
        fragment
      ) ;%make-uri-record
    ) ;define*

    (define (string->uri str)
      (make-uri str)
    ) ;define

  ) ;begin
) ;define-library
