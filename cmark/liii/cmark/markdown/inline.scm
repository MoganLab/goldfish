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
  (import (scheme char))
  (import (liii cmark markdown util))
  (import (liii cmark markdown validate))
  (import (liii cmark markdown text))
  (import (liii cmark markdown code))
  (import (only (liii cmark text) string-join string-prefix?))
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
    ;; 输出多行文本；换行后的首个文本项按行首规则转义。
    ;; 贯穿 prev 状态以检测相邻强调定界冲突；line-start 上下文持续到
    ;; 第一个非空渲染片段（空串项不消耗行首状态）。换行后 prev 与
    ;; line-start 均重置。
    (define (render-paragraph-inlines items)
      (string-split-lines (apply string-append (%render-paragraph-loop items #t '() ""))
      ) ;string-split-lines
    ) ;define

    ;; 段落状态循环：返回反向的渲染片段列表。line-start? 上下文与 prev
    ;; 贯穿；换行后重置。
    (define (%render-paragraph-loop rem line-start? acc prev)
      (cond ((null? rem) (reverse acc))
            (else
              (let* ((node (car rem))
                     (next (and (pair? (cdr rem)) (cadr rem)))
                     (r (%render-paragraph-item node line-start? prev next))
                    ) ;
                (%render-paragraph-loop (cdr rem)
                  (if (cdr r) #t (and line-start? (string=? (car r) "")))
                  (cons (car r) acc)
                  (if (cdr r) "" (car r))
                ) ;%render-paragraph-loop
              ) ;let*
            ) ;else
      ) ;cond
    ) ;define

    ;; 渲染段落中单个项。返回 (渲染结果 . 是否为换行节点)。
    ;; 换行节点行首输出空串；非换行节点按上下文渲染并贯穿 prev/next。
    (define (%render-paragraph-item node line-start? prev next)
      (when (pair? node)
        (check-inline-node node)
      ) ;when
      (let ((is-break (and (pair? node) (memq (car node) '(soft-break line-break)))))
        (if is-break
          (cons
            (if line-start? "" (if (eq? (car node) 'line-break) "  \n" "\n"))
            #t
          ) ;cons
          (cons (%render-inline-item-state node
                  (if line-start? 'paragraph-line-start 'paragraph)
                  '(* ** _ __)
                  prev
                  next
                ) ;%render-inline-item-state
            #f
          ) ;cons
        ) ;if
      ) ;let
    ) ;define

    ;; ---- 内部：分派 ----
    ;; context：normal / line-start / paragraph-line-start / paragraph /
    ;;          table-cell
    ;; delims：保留参数（不再使用，定界符由 prev/inner/next 推断）
    (define (%render-inline-items items context)
      (%render-inline-items-state items context '(* ** _ __))
    ) ;define

    ;; 贯穿 prev（前一已渲染片段）与 next（后一原始项）的状态循环。
    ;; line-start 上下文持续到首个非空渲染片段：空串项不消耗行首状态。
    (define (%render-inline-items-state items context delims)
      (apply string-append
        (let loop
          ((rem items) (first? #t) (line-start? #f) (acc '()) (prev ""))
          (cond ((null? rem) (reverse acc))
                (else
                  (let* ((item-context
                           (cond ((and (or first? line-start?) (eq? context 'line-start)) 'line-start)
                                 ((eq? context 'table-cell) 'table-cell)
                                 ((eq? context 'line-start) 'normal)
                                 (else context)
                           ) ;cond
                         ) ;item-context
                         (next (if (pair? (cdr rem)) (cadr rem) #f))
                         (rendered (%render-inline-item-state (car rem) item-context delims prev next))
                        ) ;
                    (loop (cdr rem)
                      #f
                      (and (eq? context 'line-start) (or first? line-start?) (string=? rendered ""))
                      (cons rendered acc)
                      rendered
                    ) ;loop
                  ) ;let*
                ) ;else
          ) ;cond
        ) ;let
      ) ;apply
    ) ;define

    (define (%render-inline-item item context)
      (%render-inline-item-state item context '(* ** _ __) "" #f)
    ) ;define

    ;; prev：前一已渲染片段（判断开标记与前文冲突）
    ;; next：后一原始项（判断闭标记与后文冲突，特别是下划线闭标记右侧）
    (define (%render-inline-item-state item context delims prev next)
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
               ((em) (%render-emphasis item context delims prev next))
               ((strong) (%render-strong item context delims prev next))
               ((strikeout) (%render-strikeout item context prev next))
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
    ;; 渲染策略：递归渲染内部内容（同样贯穿 prev/next 状态），再根据内部
    ;; 首尾字符、前文 prev 与后文 next 在星号（* / **）与下划线（_ / __）
    ;; 两个家族之间选择外层定界符，避免相邻标记合并成更长游程。
    ;; 先试星号（无字母数字边界限制），不可行时试下划线，两者都不可行才报错。
    (define (render-emphasis-inner item context)
      (let ((inner-context (if (paragraph-line-context? context) 'paragraph 'normal)))
        (%render-inline-items-state (cdr item) inner-context '(* ** _ __))
      ) ;let
    ) ;define

    ;; 单字符定界符（em）选择：在星号 * 与下划线 _ 之间二选一。
    ;; inner 尾部星号游程若为 1 个会与闭标记 * 合并为 **（被识别为粗体闭），
    ;; 故改用 _；尾部 2 个以上星号（闭合的粗体）与外层 * 合并为 ***，
    ;; 仍能正确解析为 em+strong，可继续用 *。前文 prev 尾为 * 时开标记合并，
    ;; 同样改用 _。下划线额外要求开/闭标记两侧非字母数字。
    (define (choose-em-delim inner prev next)
      (let ((star-run (trailing-run inner #\*))
            (under-run (trailing-run inner #\_))
            (prev-tail (char-at prev (- (string-length prev) 1)))
            (next-head (next-first-char next))
           ) ;
        (define (star-ok?)
          (and (not (eq? prev-tail #\*)) (or (= star-run 0) (>= star-run 2)))
        ) ;define
        (define (under-ok?)
          (and (not (eq? prev-tail #\_))
            (or (= under-run 0) (>= under-run 2))
            ;; 下划线开标记左侧须非字母数字或行首
            (or (not prev-tail) (not (word-char? prev-tail)))
            ;; 下划线闭标记右侧须非字母数字或行末
            (or (not next-head) (not (word-char? next-head)))
          ) ;and
        ) ;define
        (cond ((star-ok?) "*")
              ((under-ok?) "_")
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; 双字符定界符（strong）选择：在星号 ** 与下划线 __ 之间二选一。
    ;; inner 尾部只要有星号游程就会与闭标记 ** 合并为更长游程（*** 等），
    ;; 干扰粗体边界，故改用 __；前文 prev 尾为 * 同样改用 __。
    (define (choose-strong-delim inner prev next)
      (let ((star-run (trailing-run inner #\*))
            (under-run (trailing-run inner #\_))
            (prev-tail (char-at prev (- (string-length prev) 1)))
            (next-head (next-first-char next))
           ) ;
        (define (star-ok?)
          (and (not (eq? prev-tail #\*)) (= star-run 0))
        ) ;define
        (define (under-ok?)
          (and (not (eq? prev-tail #\_))
            (or (= under-run 0) (>= under-run 2))
            (or (not prev-tail) (not (word-char? prev-tail)))
            (or (not next-head) (not (word-char? next-head)))
          ) ;and
        ) ;define
        (cond ((star-ok?) "**")
              ((under-ok?) "__")
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; 字符串尾部指定字符的连续游程长度
    (define (trailing-run str ch)
      (let loop
        ((i (- (string-length str) 1)) (n 0))
        (cond ((< i 0) n)
              ((char=? (string-ref str i) ch) (loop (- i 1) (+ n 1)))
              (else n)
        ) ;cond
      ) ;let
    ) ;define

    ;; next 原始项的首个渲染字符预估：纯字符串取其首字符，结构节点难以
    ;; 预估返回 #f（保守认为右侧无字母数字限制触发）。#f 表示行末或未知。
    (define (next-first-char next)
      (cond ((not next) #f)
            ((string? next) (char-at next 0))
            (else #f)
      ) ;cond
    ) ;define

    (define (word-char? ch)
      (or (char-alphabetic? ch) (char-numeric? ch))
    ) ;define

    ;; 取字符串指定位置字符；越界返回 #f
    (define (char-at str i)
      (if (or (< i 0) (>= i (string-length str))) #f (string-ref str i))
    ) ;define

    (define (%render-emphasis item context delims prev next)
      (let* ((inner (render-emphasis-inner item context))
             (delim (choose-em-delim inner prev next))
            ) ;
        (if delim
          (string-append delim inner delim)
          (md-stem-error 'em
            "强调内容首尾与定界符冲突，无法安全往返: "
            inner
          ) ;md-stem-error
        ) ;if
      ) ;let*
    ) ;define

    (define (%render-strong item context delims prev next)
      (let* ((inner (render-emphasis-inner item context))
             (delim (choose-strong-delim inner prev next))
            ) ;
        (if delim
          (string-append delim inner delim)
          (md-stem-error 'strong
            "粗体内容首尾与定界符冲突，无法安全往返: "
            inner
          ) ;md-stem-error
        ) ;if
      ) ;let*
    ) ;define

    ;; 删除线：增加相邻防护。prev 尾为 ~（前一个删除线尾）时与 ~~ 开标记
    ;; 合并为 ~~~ 或更长游程，会破坏删除线边界，需报错。
    (define (%render-strikeout item context prev next)
      (let ((prev-tail (char-at prev (- (string-length prev) 1))))
        (if (eq? prev-tail #\~)
          (md-stem-error 'strikeout
            "相邻删除线定界游程冲突，无法安全往返: "
            prev
          ) ;md-stem-error
          (string-append "~~"
            (%render-inline-items-state (cdr item)
              (if (paragraph-line-context? context) 'paragraph 'normal)
              '(* ** _ __)
            ) ;%render-inline-items-state
            "~~"
          ) ;string-append
        ) ;if
      ) ;let
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
    ;; 链接自动链接化：title 为空且 (url==text 或 url 为 mailto:text) 时
    ;; 输出 <url> 形式自动链接，避免链接文本被重新识别为裸链接。
    ;; 其余按内联形式 [文本](url "标题") 输出。
    (define (link-autolink? url title children)
      (and (string=? title "")
        (> (string-length url) 0)
        (let ((text (apply string-append (map (lambda (x) (if (string? x) x "")) children))))
          (or (string=? url text)
            (and (string-prefix? "mailto:" url)
              (> (string-length url) 7)
              (let ((rest (substring url 7 (string-length url))))
                (string=? rest text)
              ) ;let
            ) ;and
          ) ;or
        ) ;let
      ) ;and
    ) ;define

    (define (%render-link item context)
      (let ((url (cadr item)) (title (list-ref item 2)) (children (cdddr item)))
        (if (link-autolink? url title children)
          ;; 自动链接：mailto 链接用 <text> 形式（重新解析后 url 仍为 mailto:text），
          ;; 其余 url==text 用 <url> 形式
          (let ((autolink-label (if (string-prefix? "mailto:" url) (substring url 7 (string-length url)) url)
                ) ;autolink-label
               ) ;
            (string-append "<" (escape-autolink-destination autolink-label) ">")
          ) ;let
          (string-append "["
            (%render-inline-items children 'normal)
            "]("
            (escape-link-destination url)
            (if (string=? title "") "" (string-append " \"" (escape-link-title title) "\""))
            ")"
          ) ;string-append
        ) ;if
      ) ;let
    ) ;define

    ;; 自动链接 <url> 内部：转义 < > 与反斜杠
    (define (escape-autolink-destination url)
      (%escape-chars-in url '(#\< #\> #\\))
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
