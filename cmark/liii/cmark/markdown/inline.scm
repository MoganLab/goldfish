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

;; md-stem 反向渲染的行内层：递归渲染字符串与 em/strong/code/link/
;; image/换行/数学/HTML/Emoji 等全部行内节点，按上下文参数表达块首、
;; 表格单元格、图片替代文本等差异。返回字符串。
;; 仅被 (liii cmark markdown ...) 内部模块使用，不对最终用户开放。
(define-library (liii cmark markdown inline)
  (import (scheme base))
  (import (liii cmark markdown util))
  (import (liii cmark markdown validate))
  (import (liii cmark markdown text))
  (import (liii cmark markdown code))
  (import (only (liii cmark text) string-join))
  (export render-inlines render-inlines-at-line-start render-table-cell-inlines
    render-image-alt-items render-paragraph-inlines
  ) ;export
  (begin
    ;; 渲染上下文符号：
    ;; - normal：普通行内位置
    ;; - line-start：行首（首字符影响块级识别，如 # - 1.）
    ;; - table-cell：表格单元格（额外转义管道符）

    ;; 普通行内位置渲染
    (define (render-inlines items)
      (%render-inline-items items 'normal)
    ) ;define

    ;; 行首渲染：首项若为纯文本按块首规则转义
    (define (render-inlines-at-line-start items)
      (%render-inline-items items 'line-start)
    ) ;define

    ;; 表格单元格渲染：额外转义管道符，不考虑行首规则
    (define (render-table-cell-inlines items)
      (%render-inline-items items 'table-cell)
    ) ;define

    ;; 图片替代文本：文本片段按 alt 规则，math/emoji 保留语义结构
    (define (render-image-alt-items items)
      (apply string-append
        (map
          (lambda (item)
            (cond ((string? item) (escape-image-alt-plain-text item))
                  ((and (pair? item) (eq? (car item) 'math-inline)) (render-math-inline item))
                  ((and (pair? item) (eq? (car item) 'emoji)) (render-emoji item))
                  (else (md-stem-error 'image "图片替代文本中出现了非法节点: " item)
                  ) ;else
            ) ;cond
          ) ;lambda
          items
        ) ;map
      ) ;apply
    ) ;define

    ;; 段落渲染：soft-break 渲染为换行、line-break 为两空格加换行，
    ;; 输出多行文本；换行后的首个文本项按行首规则转义
    (define (render-paragraph-inlines items)
      (let ((raw
              (apply string-append
                (let loop
                  ((rem items) (line-start? #t) (acc '()))
                  (cond ((null? rem) (reverse acc))
                        (else
                          (loop (cdr rem)
                            ;; 换行节点之后的项重新回到行首上下文
                            (and (pair? (car rem)) (memq (car (car rem)) '(soft-break line-break)))
                            (cons (%render-inline-item (car rem)
                                    (if line-start? 'paragraph-line-start 'paragraph)
                                  ) ;%render-inline-item
                              acc
                            ) ;cons
                          ) ;loop
                        ) ;else
                  ) ;cond
                ) ;let
              ) ;apply
            ) ;raw
           ) ;
        (string-split-lines raw)
      ) ;let
    ) ;define

    ;; ---- 内部：分派 ----
    (define (%render-inline-items items context)
      (apply string-append
        (let loop
          ((rem items) (first? #t) (acc '()))
          (cond ((null? rem) (reverse acc))
                (else
                  (let ((rendered
                          (%render-inline-item (car rem)
                            (if (and first? (eq? context 'line-start))
                              'line-start
                              (if (eq? context 'table-cell) 'table-cell 'normal)
                            ) ;if
                          ) ;%render-inline-item
                        ) ;rendered
                       ) ;
                    (loop (cdr rem) #f (cons rendered acc))
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;apply
    ) ;define

    (define (%render-inline-item item context)
      (cond
        ;; 纯文本：按上下文转义
        ((string? item)
         (case context
          ((line-start paragraph-line-start) (escape-text-at-line-start item))
          ((table-cell) (escape-table-cell-text item))
          (else (escape-text item))
         ) ;case
        ) ;
        ;; 结构节点：先校验再渲染
        ((pair? item)
         (check-inline-node item)
         (case (car item)
               ((em) (%render-emphasis item context))
               ((strong) (%render-strong item context))
               ((strikeout) (%render-strikeout item context))
               ((code) (%render-code-span item context))
               ((soft-break) (if (paragraph-line-context? context) "\n" " "))
               ((line-break) (if (paragraph-line-context? context) "  \n" "  "))
               ((link) (%render-link item context))
               ((image) (%render-image item))
               ((math-inline) (render-math-inline item))
               ((raw-inline) (%render-raw-inline item))
               ((emoji) (render-emoji item))
               (else (md-stem-error (car item) "未知行内节点类型: " (car item)))
         ) ;case
        ) ;
        (else (md-stem-error 'md-stem "行内内容项必须是字符串或节点: " item)
        ) ;else
      ) ;cond
    ) ;define

    (define (paragraph-line-context? context)
      (and (symbol? context)
        (or (eq? context 'paragraph-line-start) (eq? context 'paragraph))
      ) ;and
    ) ;define

    ;; ---- 强调家族 ----
    ;; em：*内容*；内容首尾是空白或 * 时边界可能被吞，转义相邻 *
    (define (%render-emphasis item context)
      (string-append "*" (%render-inline-items (cdr item) 'normal) "*")
    ) ;define

    (define (%render-strong item context)
      (string-append "**" (%render-inline-items (cdr item) 'normal) "**")
    ) ;define

    (define (%render-strikeout item context)
      (string-append "~~" (%render-inline-items (cdr item) 'normal) "~~")
    ) ;define

    ;; ---- 行内代码 ----
    ;; 表格单元格上下文中管道符转义为 \|：防止拆分单元格。
    ;; 注意：正向解析中 \| 进入代码字面量，因此含 | 的代码内容
    ;; 在表格内无法语义无损往返（正向转换也不可能产生该结构）。
    (define (%render-code-span item context)
      (let* ((literal (cadr item))
             (delim (code-span-delimiter literal))
             (content (code-span-content literal))
             (content* (if (eq? context 'table-cell) (escape-table-cell-pipes content) content)
             ) ;content*
            ) ;
        (string-append delim content* delim)
      ) ;let*
    ) ;define

    (define (escape-table-cell-pipes str)
      (let ((len (string-length str)))
        (let loop
          ((i 0) (parts '()))
          (if (>= i len)
            (string-join (reverse parts) "")
            (let ((ch (string-ref str i)))
              (if (char=? ch #\|)
                (loop (+ i 1) (cons "\\|" parts))
                (loop (+ i 1) (cons (string ch) parts))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; ---- 链接与图片 ----
    ;; 统一输出内联形式 [文本](url "标题")
    (define (%render-link item context)
      (let ((url (cadr item)) (title (list-ref item 2)) (children (cdddr item)))
        (string-append "["
          (%render-inline-items children 'normal)
          "]("
          (escape-link-destination url)
          (if (string=? title "") "" (string-append " \"" (escape-link-title title) "\""))
          ")"
        ) ;string-append
      ) ;let
    ) ;define

    (define (%render-image item)
      (let ((url (cadr item)) (title (list-ref item 2)) (alt (cdddr item)))
        (string-append "!["
          (render-image-alt-items alt)
          "]("
          (escape-link-destination url)
          (if (string=? title "") "" (string-append " \"" (escape-link-title title) "\""))
          ")"
        ) ;string-append
      ) ;let
    ) ;define

    ;; URL：含空格或 <> 时用 <...> 包裹并转义内部 < >；否则转义括号
    (define (escape-link-destination url)
      (cond ((string=? url "") "")
            ((string-index-of-any url %destination-wrapped-chars)
             (string-append "<" (%escape-destination-inside url) ">")
            ) ;
            (else (%escape-destination-parens url))
      ) ;cond
    ) ;define

    (define %destination-wrapped-chars '(#\space #\< #\>))

    (define (%escape-destination-inside url)
      (%escape-chars-in url '(#\< #\> #\\))
    ) ;define

    (define (%escape-destination-parens url)
      (%escape-chars-in url '(#\( #\)))
    ) ;define

    ;; 标题：转义 " 与 \
    (define (escape-link-title title)
      (%escape-chars-in title '(#\" #\\))
    ) ;define

    ;; ---- 数学、HTML、Emoji ----
    ;; 统一用 $...$ 输出行内数学；内容含 $ 时改用 \(...\)
    (define (render-math-inline item)
      (let ((literal (cadr item)))
        (if (> (longest-char-run #\$ literal) 0)
          (string-append "\\(" literal "\\)")
          (string-append "$" literal "$")
        ) ;if
      ) ;let
    ) ;define

    ;; 原始行内 HTML 按字面输出
    (define (%render-raw-inline item)
      (list-ref item 2)
    ) ;define

    ;; Emoji 按 shortcode 输出
    (define (render-emoji item)
      (string-append ":" (cadr item) ":")
    ) ;define

    ;; ---- 字符工具 ----
    (define (string-index-of-any str chars)
      (let ((len (string-length str)))
        (let loop
          ((i 0))
          (cond ((>= i len) #f)
                ((memq (string-ref str i) chars) i)
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%escape-chars-in str chars)
      (let ((len (string-length str)))
        (let loop
          ((i 0) (parts '()))
          (if (>= i len)
            (string-join (reverse parts) "")
            (let ((ch (string-ref str i)))
              (if (memq ch chars)
                (loop (+ i 1) (cons (string-append "\\" (string ch)) parts))
                (loop (+ i 1) (cons (string ch) parts))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
