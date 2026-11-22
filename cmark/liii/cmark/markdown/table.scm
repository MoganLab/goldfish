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

;; md-stem 反向渲染的表格层（骨架）：渲染表头、表体、对齐行和单元格。
;; 本阶段为最小骨架，表格实现随 ST5 补齐。
;; 仅被 (liii cmark markdown blocks) 使用，不对最终用户开放。
(define-library (liii cmark markdown table)
  (import (scheme base))
  (import (liii cmark markdown util))
  (import (liii cmark markdown validate))
  (import (liii cmark markdown inline))
  (export render-table)
  (begin
    ;; 表格节点 → 行列表
    (define (render-table node)
      (md-stem-error 'table "尚未支持的表格渲染")
    ) ;define
  ) ;begin
) ;define-library
