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
;; ; (liii uri internal record) - URI 底层记录类型
;; ; 仅由 (liii uri ...) 家族内部模块导入，不作为公开 API

(define-library (liii uri internal record)
  (export %make-uri-record %uri? %uri-record-scheme %uri-record-netloc
    %uri-record-path %uri-record-query %uri-record-fragment
  ) ;export
  (import (scheme base))
  (begin

    ;; ; ---------- URI 记录类型 ----------
    ;; URI 记录类型，包含五个核心组件
    ;; scheme: 协议方案（如 http, https, ftp）
    ;; netloc: 网络位置（包含 user, password, host, port）
    ;; path: 路径部分
    ;; query: 查询字符串（alist 格式）
    ;; fragment: 片段标识符
    (define-record-type <uri>
      (%make-uri-record scheme netloc path query fragment)
      %uri?
      (scheme %uri-record-scheme)
      (netloc %uri-record-netloc)
      (path %uri-record-path)
      (query %uri-record-query)
      (fragment %uri-record-fragment)
    ) ;define-record-type

  ) ;begin
) ;define-library
