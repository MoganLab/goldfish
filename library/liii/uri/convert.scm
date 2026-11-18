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
;; ; (liii uri convert) - URI 字符串转换子库
;; ; 函数文档见 bin/gf doc liii/uri 与 library/tests/liii/uri/convert/README.md

(define-library (liii uri convert)
  (import (scheme base) (liii error) (liii uri internal parse) (liii uri record))

  ;; ; ---------- 导出接口 ----------
  ;; 转换、比较与编码
  (export uri->string)
  (export uri->human-string)

  (begin

    ;; ; ---------- 转换函数 ----------
    (define (uri->string uri-obj)
      (if (not (uri? uri-obj))
        (error "uri->string: expected uri")
        (let* ((scheme (uri-scheme uri-obj))
               (netloc (uri-authority uri-obj))
               (path (uri-path uri-obj))
               (query (uri-query uri-obj))
               (fragment (uri-fragment uri-obj))
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

    (define (uri->human-string uri-obj)
      (if (not (uri? uri-obj))
        (error "uri->human-string: expected uri")
        (let* ((scheme (uri-scheme uri-obj))
               (netloc-parts (parse-netloc (uri-authority uri-obj)))
               (host (list-ref netloc-parts 2))
               (port (list-ref netloc-parts 3))
               (path (uri-path uri-obj))
              ) ;
          (string-append (if scheme (string-append scheme "://") "")
            (or host "")
            (if port (string-append ":" (number->string port)) "")
            (or path "/")
          ) ;string-append
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
