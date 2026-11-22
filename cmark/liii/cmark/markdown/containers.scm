;;
;; Copyright (C) 2026 Liii Network
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
;;

;; md-stem 反向渲染的容器层（骨架）：渲染引用、Alert、有序/无序列表、
;; 任务项的容器标记、空行、缩进，并通过 render-blocks 回调递归子块。
;; 本阶段为最小骨架，容器实现随 ST4 补齐。
;; 仅被 (liii cmark markdown blocks) 使用，不对最终用户开放。
(define-library (liii cmark markdown containers)
  (import (scheme base))
  (import (liii cmark markdown util))
  (import (liii cmark markdown validate))
  (export render-container)
  (begin
    ;; 容器节点 → 行列表；render-blocks 为块级渲染回调
    (define (render-container node render-blocks)
      (md-stem-error (car node) "尚未支持的容器节点类型: " (car node))
    ) ;define
  ) ;begin
) ;define-library
