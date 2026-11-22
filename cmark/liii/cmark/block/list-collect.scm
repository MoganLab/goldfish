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

(define-library (liii cmark block list-collect)
  (import (scheme base))
  (import (scheme char))
  (import (liii cmark text))
  (import (liii cmark block line))
  (import (liii cmark block list))
  (export collect-bullet-list-items collect-ordered-list-items
    list-indented-continuation-line? normalize-list-item-line
    normalize-bullet-list-item-line strip-list-continuation-indent
    list-tightness
  ) ;export
  (begin
    (define (collect-bullet-list-items lines)
      (collect-list-items lines markdown-unordered-list-item? collect-list-item-lines)
    ) ;define

    (define (collect-ordered-list-items lines)
      (collect-list-items lines markdown-ordered-list-item? collect-list-item-lines)
    ) ;define

    ;; 分隔线优先于列表项判定，中断列表（Example 60）
    ;; 返回 (cons (cons item-lines-list content-indents) rest)
    ;; 同一列表只收同 marker 类型/定界符的项（Example 297/301/302 换字符/分隔符断成新列表）
    (define (collect-list-items lines item-start? collect-item-lines)
      (let loop
        ((remaining lines) (items '()) (content-indents '()) (first-marker #f))
        (if
          (and (pair? remaining)
            (not (thematic-break-line? (car remaining)))
            (item-start? (car remaining))
            (or (not first-marker) (same-list-marker? (car remaining) first-marker))
          ) ;and
          (let* ((parsed (collect-item-lines remaining))
                 (item-lines (car parsed))
                 (item-content-indent (cadr parsed))
                 (rest-lines (cddr parsed))
                 (first-marker (or first-marker (parse-markdown-list-marker (car remaining))))
                ) ;
            (loop rest-lines
              (cons item-lines items)
              (cons item-content-indent content-indents)
              first-marker
            ) ;loop
          ) ;let*
          (cons (cons (reverse items) (reverse content-indents)) remaining)
        ) ;if
      ) ;let
    ) ;define

    ;; 同列表判定（跨 item）：类型与定界符/字符一致（Example 297/301/302）
    (define (same-list-marker? line marker)
      (let ((m (parse-markdown-list-marker line)))
        (and m
          (eq? (markdown-list-marker-type m) (markdown-list-marker-type marker))
          (eq? (markdown-list-marker-delimiter m) (markdown-list-marker-delimiter marker))
        ) ;and
      ) ;let
    ) ;define

    (define (collect-list-item-lines lines)
      (let* ((first-line (car lines))
             (first-marker (parse-markdown-list-marker first-line))
             (content-indent (markdown-list-marker-content-indent first-marker))
             (empty-item? (string=? (markdown-list-marker-body first-marker) ""))
            ) ;
        (let loop
          ((remaining (cdr lines))
           (collected (if empty-item? '() (list (markdown-list-marker-body first-marker))))
           (after-blank? #f)
          ) ;
          (cond
           ((null? remaining)
            (cons (trim-trailing-blank-lines (reverse collected)) (cons content-indent '()))
           ) ;
           ;; 同列表新项：缩进 < content-indent 且同标记（Example 294 vs 295/311）
           ((same-list-item? (car remaining) first-marker)
            (cons (reverse collected) (cons content-indent remaining))
           ) ;
           ;; 标记行但非同一列表（类型/定界符不同）：结束列表（Example 297/301/302）
           ((and (list-item-marker-line? (car remaining))
              (< (count-leading-spaces (car remaining)) content-indent)
            ) ;and
            (finish-list-item collected content-indent remaining)
           ) ;
           ;; 缩进不足的顶层分隔线中断列表项（Example 57）
           ((and (thematic-break-line? (car remaining))
              (< (count-leading-spaces (car remaining)) content-indent)
            ) ;and
            (finish-list-item collected content-indent remaining)
           ) ;
           ((blank-or-whitespace-line? (car remaining))
            (loop (cdr remaining) (cons "" collected) #t)
           ) ;
           ;; 空行后：空 item 只能被同列表 marker 续接（Example 280/315）；
           ;; 非空 item 要求缩进 >= content-indent（Example 257/276/313）
           ((and after-blank?
              (if empty-item?
                (not (list-item-marker-line? (car remaining)))
                (< (count-leading-spaces (car remaining)) content-indent)
              ) ;if
            ) ;and
            (finish-list-item collected content-indent remaining)
           ) ;
           (else
             (loop (cdr remaining)
               (cons (normalize-list-item-line (car remaining) content-indent) collected)
               #f
             ) ;loop
           ) ;else
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define (trim-trailing-blank-lines lines)
      (reverse (trim-leading-blank-lines (reverse lines)))
    ) ;define

    ;; 结束当前 item：剥尾部空行后连同 content-indent 与剩余行返回
    (define (finish-list-item collected content-indent remaining)
      (cons (trim-trailing-blank-lines (reverse collected))
        (cons content-indent remaining)
      ) ;cons
    ) ;define

    (define (trim-leading-blank-lines lines)
      (cond ((null? lines) '())
            ((blank-or-whitespace-line? (car lines)) (trim-leading-blank-lines (cdr lines)))
            (else lines)
      ) ;cond
    ) ;define

    (define (list-indented-continuation-line? line . maybe-indent)
      (let ((required-indent (if (null? maybe-indent) 2 (car maybe-indent))))
        (or (and (> (string-length line) 0) (char=? (string-ref line 0) #\tab))
          (let ((spaces (count-leading-spaces line)))
            (or (>= spaces required-indent) (>= spaces 2))
          ) ;let
        ) ;or
      ) ;let
    ) ;define

    (define (normalize-list-item-line line . maybe-indent)
      (let ((required-indent (if (null? maybe-indent) 2 (car maybe-indent))))
        (if (list-indented-continuation-line? line required-indent)
          (strip-list-continuation-indent line required-indent)
          line
        ) ;if
      ) ;let
    ) ;define

    (define (normalize-bullet-list-item-line line)
      (normalize-list-item-line line 2)
    ) ;define

    ;; 剥 content-indent；缩进不足的行不剥（保持原样，交给块解析按段落续行处理，
    ;; 避免剥 2 后把 marker 行误判为嵌套列表，Example 312）
    (define (strip-list-continuation-indent line . maybe-indent)
      (let ((required-indent (if (null? maybe-indent) 2 (car maybe-indent))))
        (cond
         ((and (> (string-length line) 0) (char=? (string-ref line 0) #\tab))
          (substring line 1)
         ) ;
         ((>= (count-leading-spaces line) required-indent)
          (substring line required-indent)
         ) ;
         (else line)
        ) ;cond
      ) ;let
    ) ;define

    ;; 围栏开/闭行（tightness 扫描用；与 block-collector 的 parse-fence-line 等价判定）
    (define (fence-line? line)
      (let* ((len (string-length line)) (indent (count-leading-spaces line)))
        (and (<= indent 3)
          (< indent len)
          (let ((c (string-ref line indent)))
            (and (or (char=? c #\`) (char=? c #\~))
              (let loop
                ((i (+ indent 1)))
                (if (and (< i len) (char=? (string-ref line i) c))
                  (loop (+ i 1))
                  (and (>= (- i indent) 3) (string=? (string-trim (substring line i len)) ""))
                ) ;if
              ) ;let
            ) ;and
          ) ;let
        ) ;and
      ) ;let*
    ) ;define

    ;; tight/loose 判定（行层面）：
    ;; - 围栏代码内的空行不计（Example 318）
    ;; - 空行后首个非空行是 fence 开行（后有匹配闭行）→ 空行在段落与围栏之间 → loose（Example 263）
    ;; - 空行前一行是嵌套列表 marker → 空行属更深层，不计（Example 307/319）
    ;; - 其余空行：剥 content-indent 后 < 4 空格（段落/缩进代码）→ loose（Example 258/264/270/316）；
    ;;   剥后 >= 4 空格（更深嵌套）→ 不计
    ;; - item 行尾空行（被 same-list 分支保留）→ loose（Example 306）
    (define (list-tightness item-lines-list content-indents)
      (if (any-item-has-blank-line? item-lines-list content-indents) 'loose 'tight)
    ) ;define

    (define (any-item-has-blank-line? item-lines-list content-indents)
      (cond ((null? item-lines-list) #f)
            ((list-item-has-blank-line? (car item-lines-list) (car content-indents)) #t)
            (else (any-item-has-blank-line? (cdr item-lines-list) (cdr content-indents)))
      ) ;cond
    ) ;define

    (define (list-item-has-blank-line? lines content-indent)
      (let loop
        ((remaining lines) (in-fence? #f) (prev-line #f))
        (cond ((null? remaining) #f)
              ((fence-line? (car remaining))
               (loop (cdr remaining) (not in-fence?) (car remaining))
              ) ;
              ((blank-or-whitespace-line? (car remaining))
               (if in-fence?
                 (loop (cdr remaining) #t prev-line)
                 (blank-line-loosens? (cdr remaining) content-indent prev-line)
               ) ;if
              ) ;
              (else (loop (cdr remaining) in-fence? (car remaining)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 围栏行长（indent 后的连续 fence 字符数）
    (define (fence-length line)
      (let* ((indent (count-leading-spaces line))
             (len (string-length line))
             (c (string-ref line indent))
            ) ;
        (let loop
          ((i (+ indent 1)))
          (if (and (< i len) (char=? (string-ref line i) c)) (loop (+ i 1)) (- i indent))
        ) ;let
      ) ;let*
    ) ;define

    ;; 后续行中存在匹配闭行（同字符、长度 >= 开行）→ 该 fence 行是开行
    (define (fence-has-closing? line rest)
      (let* ((indent (count-leading-spaces line))
             (c (string-ref line indent))
             (n (fence-length line))
            ) ;
        (let loop
          ((remaining rest))
          (cond ((null? remaining) #f)
                ((and (fence-line? (car remaining))
                   (let ((i2 (count-leading-spaces (car remaining))))
                     (and (char=? (string-ref (car remaining) i2) c)
                       (>= (fence-length (car remaining)) n)
                     ) ;and
                   ) ;let
                 ) ;and
                 #t
                ) ;
                (else (loop (cdr remaining)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define (blank-line-loosens? lines content-indent prev-line)
      (cond ((null? lines) #t)
            ((blank-or-whitespace-line? (car lines))
             (blank-line-loosens? (cdr lines) content-indent prev-line)
            ) ;
            ((fence-line? (car lines))
             (if (fence-has-closing? (car lines) (cdr lines)) #t #f)
            ) ;
            ;; 空行后的行（已剥当前层 content-indent）：
            ;; - 空行前是嵌套 marker：后行缩进 < 2（回到当前层段落）才 loose（Example 325），
            ;;   否则属更深层嵌套不计（Example 307/319）
            ;; - 空行前非 marker：0-1 空格段落或 >= 4 空格缩进代码 → 当前层直接块 → loose
            ;;   （Example 258/264/270/316）；2-3 空格（嵌套 continuation）不计
            (else
              (let ((indent (count-leading-spaces (car lines))))
                (if (and prev-line (list-item-marker-line? prev-line))
                  (< indent 2)
                  (or (< indent 2) (>= indent 4))
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library
