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

    ;; gfproject-root : (-> (or #f string?))
    (define hint-gfproject-root '((-> (or #f string?))))

    ;; gfproject-get-gf-lib : (-> string?)
    (define hint-gfproject-get-gf-lib '((-> string?)))

    ;; gfproject--opt-gf-lib : (list? -> string?)
    (define hint-gfproject--opt-gf-lib '((list? -> string?)))

    ;; 配置文件查找：可选参数为库目录。
    (define hint-gfproject-find-lib-path
      '((-> (or #f string?)) (string? -> (or #f string?)))
    ) ;define
    (define hint-gfproject-find-local-path '((-> (or #f string?))))

    ;; JSON 配置统一使用 any?，避免把空配置 '(()) 误标为普通 alist。
    (define hint-gfproject-read-file '((any? -> any?)))
    (define hint-gfproject-extract-tools '((any? -> any?)))
    (define hint-gfproject-deep-merge '((any? any? -> any?)))
    (define hint-gfproject-load-config-bundle '((-> list?) (string? -> list?)))
    (define hint-gfproject-load-config '((-> any?) (string? -> any?)))

    ;; bundle-ref : (alist? any? -> any?)
    ;; 命中返回 cdr 值，未命中返回 #f；(or #f any?) 等价于 any?
    (define hint-bundle-ref '((alist? any? -> any?)))

    (define (hint-liii/gfproject/config name)
      (case name
       ((gfproject-root) hint-gfproject-root)
       ((gfproject-get-gf-lib) hint-gfproject-get-gf-lib)
       ((gfproject--opt-gf-lib) hint-gfproject--opt-gf-lib)
       ((gfproject-find-lib-path) hint-gfproject-find-lib-path)
       ((gfproject-find-local-path) hint-gfproject-find-local-path)
       ((gfproject-read-file) hint-gfproject-read-file)
       ((gfproject-extract-tools) hint-gfproject-extract-tools)
       ((gfproject-deep-merge) hint-gfproject-deep-merge)
       ((gfproject-load-config-bundle) hint-gfproject-load-config-bundle)
       ((gfproject-load-config) hint-gfproject-load-config)
       ((bundle-ref) hint-bundle-ref)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
