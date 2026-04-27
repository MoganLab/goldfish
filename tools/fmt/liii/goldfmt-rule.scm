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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldfmt-rule)
  (export max-inline-length
          must-inline?
          never-inline?
          never-inline-when-first-child-env?
          allow-first-line-child-env?
          first-line-limit
          rest-indent
          path-has-common-ancestor?
          find-node-rules-paths
          find-node-rules-path)
  (import (liii base)
          (liii json)
          (liii os)
          (liii path)
          (liii string)
          (liii sys))

  (begin
    (define max-inline-length 80)

    ;; 兼容：旧的硬编码相对路径，作为 fallback 使用
    (define node-rules-paths
      '("node-rules.json"
        "../node-rules.json"
        "../../node-rules.json"
        "../../../node-rules.json"))

    (define *node-rules* #f)

    ;; 辅助函数：将路径转为各部分列表（去掉空字符串部分）
    (define (path-parts-list p)
      (let ((parts (path-parts (path p))))
        (let loop ((i 0) (result '()))
          (if (>= i (vector-length parts))
            (reverse result)
            (let ((part (vector-ref parts i)))
              (if (string-null? part)
                (loop (+ i 1) result)
                (loop (+ i 1) (cons part result))))))))

    ;; 判断两个路径是否有共同祖先（排除根目录 '/'）
    (define (path-has-common-ancestor? path1 path2)
      (let ((parts1 (path-parts-list path1))
            (parts2 (path-parts-list path2)))
        (let loop ((p1 parts1) (p2 parts2) (common 0))
          (cond ((or (null? p1) (null? p2))
                 (> common 0))
                ((string=? (car p1) (car p2))
                 (loop (cdr p1) (cdr p2) (+ common 1)))
                (else
                 (> common 0))))))

    ;; 在 *load-path* 中查找与 executable 有共同祖先的路径中的 node-rules.json
    ;; 同时检查 load-path 本身及其父目录（参考 doc 工具的 sibling 策略）
    (define (find-node-rules-paths)
      (let ((exe (executable)))
        (let loop ((paths *load-path*) (result '()))
          (if (null? paths)
            result
            (let ((load-root (car paths)))
              (if (and (string? load-root)
                       (path-has-common-ancestor? exe load-root))
                (let* ((direct (path->string (path-join load-root "node-rules.json")))
                       (parent-str (path->string (path-parent (path load-root))))
                       (parent (path->string (path-join parent-str "node-rules.json")))
                       (found (cond ((path-file? direct) direct)
                                    ((path-file? parent) parent)
                                    (else #f))))
                  (if found
                    (loop (cdr paths) (append result (list found)))
                    (loop (cdr paths) result)))
                (loop (cdr paths) result)))))))

    ;; 查找 node-rules.json 路径
    ;; 优先使用 *load-path* 中与 executable 有重叠的路径，fallback 到旧逻辑
    (define (find-node-rules-path)
      (let ((paths (find-node-rules-paths)))
        (if (null? paths)
          (let loop ((legacy-paths node-rules-paths))
            (cond
              ((null? legacy-paths) "node-rules.json")
              ((access (car legacy-paths) 'R_OK) (car legacy-paths))
              (else (loop (cdr legacy-paths)))))
          (car paths))))

    (define (node-rules)
      (if *node-rules*
          *node-rules*
          (let ((rules (string->json (path-read-text (find-node-rules-path)))))
            (set! *node-rules* rules)
            rules)))

    (define (rule-for tag-name)
      (let* ((rules (node-rules))
             (rule (json-ref rules tag-name)))
        (if (null? rule)
            (json-ref rules "default")
            rule)))

    (define (rule-ref tag-name field default-value)
      (let* ((rules (node-rules))
             (rule (rule-for tag-name))
             (value (json-ref rule field)))
        (if (null? value)
            (let ((default-rule (json-ref rules "default")))
              (let ((default-field (json-ref default-rule field)))
                (if (null? default-field)
                    default-value
                    default-field)))
            value)))

    (define (json-bool value)
      (cond
        ((eq? value #t) #t)
        ((eq? value #f) #f)
        ((eq? value 'true) #t)
        ((eq? value 'false) #f)
        (else #f)))

    (define (rule-bool tag-name field default-value)
      (json-bool (rule-ref tag-name field default-value)))

    (define (must-inline? tag-name)
      (rule-bool tag-name "mustInline" #f))

    (define (never-inline? tag-name)
      (rule-bool tag-name "neverInline" #f))

    (define (never-inline-when-first-child-env? tag-name)
      (rule-bool tag-name "neverInlineWhenFirstChildEnv" #f))

    (define (allow-first-line-child-env? tag-name)
      (rule-bool tag-name "allowFirstLineChildEnv" #t))

    (define (first-line-limit tag-name)
      (rule-ref tag-name "firstLineLimit" 1))

    (define (rest-indent tag-name)
      (let ((value (rule-ref tag-name "restIndent" "byFirstRestChild")))
        (cond
          ((string=? value "alignToFirstSelectedEnv")
           'align-to-first-selected-env)
          ((string=? value "parentPlus2")
           'parent-plus2)
          (else 'by-first-rest-child))))
    ))
