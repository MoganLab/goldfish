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
;; ; (liii uri) - URI 处理库统一公开入口
;; ; 函数分类索引见 bin/gf doc liii/uri（library/tests/liii/uri/README.md）

(define-library (liii uri)
  (import (scheme base)
    (liii uri record)
    (liii uri predicate)
    (liii uri compare)
    (liii uri make)
    (liii uri transform)
    (liii uri convert)
  ) ;import

  ;; ; ---------- 导出接口 ----------
  ;; 构造 URI
  (export make-uri string->uri)
  (export make-uri-raw)
  (export uri-build)

  ;; 访问 URI 组件
  (export uri-scheme uri-raw-scheme)
  (export uri-host uri-raw-host uri-port uri-explicit-port)
  (export uri-user uri-raw-user uri-password uri-raw-password)
  (export uri-authority uri-raw-authority)
  (export uri-path uri-raw-path uri-path->list)
  (export uri-query uri-query-string uri-query-ref uri-query-ref*)
  (export uri-fragment uri-raw-fragment)
  (export uri-parent uri-name uri-suffix uri-suffixes)

  ;; 修改 URI
  (export uri-with-scheme uri-with-host uri-with-port)
  (export uri-with-path uri-with-fragment)
  (export uri-update-query uri-extend-query)
  (export uri-without-query uri-without-query-param)
  (export uri-join-path uri-join)
  (export uri-with-name uri-with-suffix)
  (export uri-normalize)

  ;; 转换、比较与编码
  (export uri->string)
  (export uri->human-string)
  (export uri=? uri<? uri>?)
  (export uri-hash)
  (export uri-encode uri-decode)
  (export uri-encode-path uri-decode-path)
  (export query-string->alist alist->query-string)

  ;; 谓词
  (export uri?)
  (export uri-absolute? uri-relative?)
  (export uri-default-port? uri-network-scheme?)

  ;; 端口
  (export uri-default-port)

  ;; 错误
  (export uri-error)
) ;define-library
