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

;; (hint liii gfproject config) - (liii gfproject config) 的类型接口
;;
;; 本库只提供类型签名数据，不导出 (liii gfproject config) 的任何过程，
;; 也不导入其他 hint 库，因此可以被独立加载。
;;
;; 登记接口的目的是将该库纳入 gf check type-hint 的文件级检查：
;; 原库源码体内的调用会依据 import 依赖（如 (scheme base) 的
;; assoc、cdr）的 hint 做实参类型检查。

(define-library (hint liii gfproject config)
  (import (scheme base))
  (export hint-liii/gfproject/config)
  (begin

    ;; bundle-ref : (any? symbol? -> any?)
    (define hint-bundle-ref '((any? symbol? -> any?)))

    (define (hint-liii/gfproject/config name)
      (case name
       ((bundle-ref) hint-bundle-ref)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
