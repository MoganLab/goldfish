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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; (hint liii goldfmt format) - (liii goldfmt format) 的类型接口

(define-library (hint liii goldfmt format)
  (import (scheme base))
  (export hint-liii/goldfmt/format)
  (begin

    ;; format-datum : (any? -> string?)
    (define hint-format-datum '((any? -> string?)))

    ;; format-datum+node : (any? -> any?)
    ;; 返回格式化文本与定位后节点两个值，多返回值暂不能表达。
    (define hint-format-datum+node '((any? -> any?)))

    ;; format-node : (any? integer? -> any?)
    (define hint-format-node '((any? integer? -> any?)))

    ;; format-string : (string? -> string?)
    (define hint-format-string '((string? -> string?)))

    ;; format-nodes : (vector? -> string?)
    (define hint-format-nodes '((vector? -> string?)))

    ;; format-inline 与 can-inline? 定义于 (liii goldfmt format writer)，
    ;; 经再导出穿透在 (hint liii goldfmt format writer) 中登记。

    (define (hint-liii/goldfmt/format name)
      (case name
       ((format-datum) hint-format-datum)
       ((format-datum+node) hint-format-datum+node)
       ((format-node) hint-format-node)
       ((format-string) hint-format-string)
       ((format-nodes) hint-format-nodes)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
