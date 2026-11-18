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
;; ; (liii uri predicate) - URI 谓词子库
;; ; 函数文档见 bin/gf doc liii/uri 与 library/tests/liii/uri/predicate/README.md

(define-library (liii uri predicate)
  (import (scheme base) (liii uri internal parse) (liii uri record))

  ;; ; ---------- 导出接口 ----------
  ;; 谓词
  (export uri-absolute? uri-relative?)
  (export uri-default-port? uri-network-scheme?)

  ;; 端口
  (export uri-default-port)

  (begin

    ;; ; ---------- 谓词函数 ----------
    (define (uri-absolute? uri-obj)
      (and (uri? uri-obj)
        (uri-scheme uri-obj)
        (not (string=? (uri-scheme uri-obj) ""))
      ) ;and
    ) ;define

    (define (uri-relative? uri-obj)
      (and (uri? uri-obj)
        (or (not (uri-scheme uri-obj)) (string=? (uri-scheme uri-obj) ""))
      ) ;and
    ) ;define

    (define (uri-default-port? uri-obj)
      (let* ((scheme (uri-scheme uri-obj))
             (default-port (and scheme (%uri-default-port scheme)))
             (explicit-port (uri-explicit-port uri-obj))
            ) ;
        (and default-port (or (not explicit-port) (= explicit-port default-port)))
      ) ;let*
    ) ;define

    (define (uri-network-scheme? scheme)
      (and (%uri-default-port scheme) #t)
    ) ;define

    ;; ; ---------- 端口 ----------
    (define (uri-default-port scheme)
      (%uri-default-port scheme)
    ) ;define

  ) ;begin
) ;define-library
