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

;; md-stem 反向渲染的块级分派：把已校验的块节点列表渲染为按逻辑行
;; 组织的字符串列表。本阶段为最小骨架：先校验再分派，未实现的合法
;; 节点显式报 unsupported-node，不返回空字符串。
;; 仅被 (liii cmark markdown ...) 内部模块使用，不对最终用户开放。
(define-library (liii cmark markdown blocks)
  (import (scheme base))
  (import (liii cmark markdown util))
  (import (liii cmark markdown validate))
  (export render-blocks)
  (begin
    ;; 单个块节点 → 行列表（骨架：ST3 起逐类实现）
    (define (render-block node)
      (check-block-node node)
      (let ((type (car node)))
        (case type
          (else (md-stem-error type "尚未支持的块节点类型: " type))
        ) ;case
      ) ;let
    ) ;define

    ;; 块节点列表 → 行列表；兄弟块之间的空行由 document 统一插入
    (define (render-blocks nodes)
      (append-line-lists (map render-block nodes))
    ) ;define
  ) ;begin
) ;define-library
