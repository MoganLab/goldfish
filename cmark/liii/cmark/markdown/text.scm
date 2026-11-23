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

;; md-stem 反向渲染的普通文本上下文转义：按渲染位置选择需要转义的
;; 字符集合，防止文本被重新识别为 CommonMark 或项目扩展语法
;; （$..$ 数学、~~删除线~~、:emoji:、裸链接）。
;; 仅被 (liii cmark markdown ...) 内部模块使用，不对最终用户开放。
(define-library (liii cmark markdown text)
  (import (scheme base))
  (import (scheme char))
  (import (liii cmark markdown util))
  (import (only (liii cmark text) string-join string-prefix-at?))
  (export escape-text escape-text-at-line-start escape-table-cell-text
    escape-image-alt-plain-text
  ) ;export
  (begin
    ;; 行内任意位置都需要转义的 ASCII 标点（CommonMark 能识别的标记字符
    ;; 加项目扩展语法字符）：\ ` * _ [ ] < > 以及 $ ~（数学、删除线）。
    ;; Emoji 冒号与裸 URL 的 scheme 冒号由 %escape-with 中的规则处理。
    (define %always-escape-chars '(#\\ #\` #\* #\_ #\[ #\] #\< #\> #\$ #\~))

    ;; 表格单元格额外转义管道符
    (define %table-cell-escape-chars (cons #\| %always-escape-chars))

    ;; 普通行内文本转义：backslash-escape 所有标记字符。
    ;; 反斜杠本身在最前，避免二次转义。
    (define (escape-text str)
      (%escape-with %always-escape-chars str)
    ) ;define

    ;; 块首文本（段落第一行、标题等）额外防止：
    ;; # 标题、-/+/* 列表与分隔线、> 引用、数字. 列表
    (define (escape-text-at-line-start str)
      (if (string=? str "")
        ""
        (let ((head (string-ref str 0)))
          (cond
           ((char=? head #\#)
            (string-append "\\#"
              (%escape-with %always-escape-chars (substring str 1 (string-length str)))
            ) ;string-append
           ) ;
           ((or (char=? head #\-) (char=? head #\+) (char=? head #\*))
            (string-append "\\"
              (string head)
              (%escape-with %always-escape-chars (substring str 1 (string-length str)))
            ) ;string-append
           ) ;
           ((char=? head #\>)
            (string-append "\\>"
              (%escape-with %always-escape-chars (substring str 1 (string-length str)))
            ) ;string-append
           ) ;
           ((char=? head #\=)
            ;; 行首 ===/===... 为 setext h1 下划线，转义首字符
            (string-append "\\="
              (%escape-with %always-escape-chars (substring str 1 (string-length str)))
            ) ;string-append
           ) ;
           ((and (char-numeric? head) (starts-with-list-marker? str))
            ;; 数字 + ./) 开头：转义第一个标记字符
            (escape-leading-list-marker str)
           ) ;
           (else (%escape-with %always-escape-chars str))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;; 表格单元格文本：额外转义管道符，其余同普通行内
    (define (escape-table-cell-text str)
      (%escape-with %table-cell-escape-chars str)
    ) ;define

    ;; 图片替代文本中的纯文本片段：不出现在 Markdown 语法位置，
    ;; 但 ] 与 [ 会提前终止 alt，需转义；其余标记字符保守转义
    (define (escape-image-alt-plain-text str)
      (%escape-with %always-escape-chars str)
    ) ;define

    ;; ---- 内部 ----
    ;; 主转义循环：chars 集合内的字符直接转义；
    ;; 冒号按上下文规则转义（Emoji shortcode 形式 :name: 与裸 URL 的 ://）；
    ;; & 在开启实体引用（&name; / &#123; / &#x1F;）时转义为 \&，
    ;; 避免反向输出后被正向解析重新解码为字符。
    (define (%escape-with chars str)
      (let ((len (string-length str)))
        (let loop
          ((i 0) (parts '()))
          (if (>= i len)
            (string-join (reverse parts) "")
            (let ((ch (string-ref str i)))
              (cond
               ((memq ch chars) (loop (+ i 1) (cons (string-append "\\" (string ch)) parts)))
               ((and (char=? ch #\:)
                  (or (colon-starts-url-scheme? str i) (colon-starts-emoji? str i))
                ) ;and
                (loop (+ i 1) (cons (string-append "\\:") parts))
               ) ;
               ((and (char=? ch #\&) (amp-starts-entity? str i))
                (loop (+ i 1) (cons "\\&" parts))
               ) ;
               (else (loop (+ i 1) (cons (string ch) parts)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 位置 i 的 & 开启实体引用：后跟字母（&name;）或 #（&#123; / &#x1F;）
    (define (amp-starts-entity? str i)
      (let ((next (char-at str (+ i 1))))
        (and next (or (char-alphabetic? next) (char=? next #\#)))
      ) ;let
    ) ;define

    ;; 取字符串指定位置字符；越界返回 #f
    (define (char-at str i)
      (if (or (< i 0) (>= i (string-length str))) #f (string-ref str i))
    ) ;define

    ;; 位置 i 的冒号开启 URL scheme：冒号后紧跟 //
    (define (colon-starts-url-scheme? str i)
      (string-prefix-at? str (+ i 1) "//")
    ) ;define

    ;; 位置 i 的冒号开启 Emoji shortcode：冒号后跟 [a-z0-9_+-]+ 且再跟冒号
    (define (colon-starts-emoji? str i)
      (let ((len (string-length str)))
        (let loop
          ((j (+ i 1)))
          (cond ((>= j len) #f)
                ((colon-after? str j) (> j (+ i 1)))
                ((shortcode-char? (string-ref str j)) (loop (+ j 1)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (colon-after? str j)
      (and (< j (string-length str)) (char=? (string-ref str j) #\:))
    ) ;define

    (define (shortcode-char? ch)
      (or (char-alphabetic? ch)
        (char-numeric? ch)
        (char=? ch #\_)
        (char=? ch #\+)
        (char=? ch #\-)
      ) ;or
    ) ;define

    ;; 数字开头后跟 "数字*. / )" 形式列表标记
    (define (starts-with-list-marker? str)
      (let ((len (string-length str)))
        (let loop
          ((i 0))
          (cond ((>= i len) #f)
                ((char-numeric? (string-ref str i)) (loop (+ i 1)))
                ((and (< i len)
                   (or (char=? (string-ref str i) #\.) (char=? (string-ref str i) #\)))
                 ) ;and
                 (> i 0)
                ) ;
                (else #f)
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; 转义列表标记字符：123. → 123\.
    (define (escape-leading-list-marker str)
      (let ((len (string-length str)))
        (let loop
          ((i 0))
          (cond ((>= i len) (%escape-with %always-escape-chars str))
                ((char-numeric? (string-ref str i)) (loop (+ i 1)))
                (else
                  (string-append (substring str 0 i)
                    (string-append "\\" (string (string-ref str i)))
                    (%escape-with %always-escape-chars (substring str (+ i 1) len))
                  ) ;string-append
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
