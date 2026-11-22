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

;; md-stem 反向渲染的块级分派：把已校验的块节点渲染为按逻辑行组织的
;; 字符串列表。本模块实现段落、标题、分隔线、代码块、数学块和原始
;; HTML 块；容器节点由 containers 渲染（通过回调回到本模块递归）。
;; 仅被 (liii cmark markdown ...) 内部模块使用，不对最终用户开放。
(define-library (liii cmark markdown blocks)
  (import (scheme base))
  (import (liii cmark markdown util))
  (import (liii cmark markdown validate))
  (import (liii cmark markdown inline))
  (import (liii cmark markdown code))
  (import (liii cmark markdown table))
  (import (liii cmark markdown containers))
  (export render-blocks render-block)
  (begin
    ;; 单个块节点 → 行列表；先校验再分派，未知节点报错
    (define (render-block node)
      (check-block-node node)
      (case (car node)
            ((p) (render-paragraph node))
            ((h1 h2 h3 h4 h5 h6) (render-heading node))
            ((thematic-break) '("---"))
            ((code-block) (render-code-block node))
            ((math-block) (render-math-block node))
            ((raw-block) (render-raw-block node))
            ((blockquote alert) (render-container-block node))
            ((bullet-list ordered-list) (render-container-block node))
            ((table) (render-table-block node))
            (else (md-stem-error (car node) "尚未支持的块节点类型: " (car node)))
      ) ;case
    ) ;define

    ;; 块节点列表 → 行列表；兄弟块之间的空行由 document 统一插入
    (define (render-blocks nodes)
      (append-line-lists (map render-block nodes))
    ) ;define

    ;; ---- 段落 ----
    ;; 段落行内内容为单行字符串（soft-break/line-break 已在行内渲染
    ;; 为空格或两空格），因此段落总是单行
    (define (render-paragraph node)
      (list (render-inlines-at-line-start (cdr node)))
    ) ;define

    ;; ---- 标题 ----
    (define (render-heading node)
      (let ((level (heading-level (car node))))
        (list (string-append (make-string level #\#)
                " "
                (render-inlines-at-line-start (cdr node))
              ) ;string-append
        ) ;list
      ) ;let
    ) ;define

    (define (heading-level type)
      (case type
       ((h1) 1)
       ((h2) 2)
       ((h3) 3)
       ((h4) 4)
       ((h5) 5)
       ((h6) 6)
       (else (md-stem-error type "未知标题级别: " type))
      ) ;case
    ) ;define

    ;; ---- 围栏代码块 ----
    ;; fence 长度按内容动态计算；info 含反引号时报错（CommonMark 规定
    ;; 反引号围栏的 info 不能含反引号）
    (define (render-code-block node)
      (let* ((info (cadr node))
             (literal (list-ref node 2))
             (fence (make-string-repeat #\` (code-fence-length literal)))
            ) ;
        (when (> (longest-char-run #\` info) 0)
          (md-stem-error 'code-block "code-block 的 info 不能包含反引号: " info)
        ) ;when
        (cons (string-append fence info)
          (append (render-literal-lines literal) (list fence))
        ) ;cons
      ) ;let*
    ) ;define

    ;; 字面内容按 \n 拆成行列表；空内容没有内容行
    (define (render-literal-lines literal)
      (if (string=? literal "") '() (string-split-lines literal))
    ) ;define

    ;; ---- 数学块 ----
    ;; 统一用 $$ 围栏输出；内容含 $$ 时报错（应转为转义定界不可行，
    ;; 数学块字面量中 $$ 会提前终止围栏）
    (define (render-math-block node)
      (let* ((literal (cadr node)) (fence "$$"))
        (when (> (longest-char-run #\$ literal) 1)
          (md-stem-error 'math-block
            "math-block 内容包含 $$，无法安全定界: "
            literal
          ) ;md-stem-error
        ) ;when
        (cons fence (append (render-literal-lines literal) (list fence)))
      ) ;let*
    ) ;define

    ;; ---- 原始 HTML 块 ----
    ;; 按字面多行输出
    (define (render-raw-block node)
      (render-literal-lines (list-ref node 2))
    ) ;define

    ;; ---- 容器与表格 ----
    ;; 容器由 containers 渲染，传入本模块的 render-blocks 作递归回调
    (define (render-container-block node)
      (render-container node render-blocks)
    ) ;define

    (define (render-table-block node)
      (render-table node)
    ) ;define
  ) ;begin
) ;define-library
