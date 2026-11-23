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

(define-library (liii cmark)
  (import (liii cmark node)
    (liii cmark parser)
    (liii cmark stem)
    (liii cmark markdown)
    (liii cmark stem-reader)
  ) ;import
  (export
    ;; stem 转换与读取
    markdown->md-stem
    md-stem->markdown
    read-md-stem
    ;; 解析入口
    parse-inlines
    parse-fenced-code-block
    parse-indented-code-block
    ;; AST 节点核心接口
    make-cmark-node
    cmark-node?
    cmark-node-type
    cmark-node-set-type!
    cmark-node-data
    cmark-node-set-data!
    cmark-node-children
    cmark-node-set-children!
    cmark-node-parent
    cmark-node-set-parent!
    cmark-block-type?
    cmark-inline-type?
    cmark-all-types
    cmark-node-append-child!
    cmark-node-first-child
    cmark-node-last-child
    cmark-node-type?
    cmark-document?
    cmark-paragraph?
    cmark-text?
    cmark-heading?
    cmark-list?
  ) ;export
) ;define-library
