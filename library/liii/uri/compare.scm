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
;; ; (liii uri compare) - URI 比较与哈希子库
;; ; 函数文档见 bin/gf doc liii/uri 与 library/tests/liii/uri/compare/README.md

(define-library (liii uri compare)
  (import (scheme base)
    (liii uri internal record)
    (liii uri record)
  ) ;import

  ;; ; ---------- 导出接口 ----------
  ;; 转换与比较
  (export uri=? uri<? uri>?)
  (export uri-hash)

  (begin

    ;; ; ---------- 比较辅助 ----------
    (define (uri->string-for-compare uri-obj)
      (if (not (%uri? uri-obj))
        (error "uri->string-for-compare: expected uri")
        (let* ((scheme (%uri-record-scheme uri-obj))
               (netloc (%uri-record-netloc uri-obj))
               (path (%uri-record-path uri-obj))
               (query (%uri-record-query uri-obj))
               (fragment (%uri-record-fragment uri-obj))
              ) ;
          (string-append (if scheme (string-append scheme ":") "")
            (if (and scheme (not (string=? netloc ""))) "//" "")
            (if (not (string=? netloc "")) netloc "")
            (or path "")
            (if (null? query) "" (string-append "?" (alist->query-string query)))
            (if fragment (string-append "#" fragment) "")
          ) ;string-append
        ) ;let*
      ) ;if
    ) ;define

    ;; ; ---------- 比较函数 ----------
    (define (uri=? uri1 uri2)
      (and (%uri? uri1)
        (%uri? uri2)
        (string=? (uri->string-for-compare uri1) (uri->string-for-compare uri2))
      ) ;and
    ) ;define

    (define (uri<? uri1 uri2)
      (and (%uri? uri1)
        (%uri? uri2)
        (string<? (uri->string-for-compare uri1) (uri->string-for-compare uri2))
      ) ;and
    ) ;define

    (define (uri>? uri1 uri2)
      (and (%uri? uri1)
        (%uri? uri2)
        (string>? (uri->string-for-compare uri1) (uri->string-for-compare uri2))
      ) ;and
    ) ;define

    ;; ; ---------- 哈希函数 ----------
    (define (uri-hash uri-obj)
      (if (not (%uri? uri-obj))
        (error "uri-hash: expected uri")
        (let ((scheme (or (%uri-record-scheme uri-obj) ""))
              (netloc (%uri-record-netloc uri-obj))
              (path (or (%uri-record-path uri-obj) ""))
              (query (%uri-record-query uri-obj))
              (fragment (or (%uri-record-fragment uri-obj) ""))
             ) ;
          (+ (string-length scheme)
            (string-length netloc)
            (string-length path)
            (length query)
            (string-length fragment)
          ) ;+
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
