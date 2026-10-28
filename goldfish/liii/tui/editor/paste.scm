;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

;; 粘贴内容折叠：超过阈值的粘贴在编辑器中只保留 [paste #N ...] 标记，
;; 原文保存在注册表里，提交时再由 paste-expand 展开。
;; 阈值：>10 行 或 >1000 字符；字符数优先于行数展示。
(define-library (liii tui editor paste)
  (export %bracketed-paste? %bracketed-paste-content %paste-normalize
    %paste-large? make-paste-registry paste-registry? paste-registry-add!
    paste-registry-clear! paste-registry-count paste-registry-remove!
    paste-registry-snapshot paste-registry-restore! paste-expand
    paste-marker-at-end paste-marker-at-start
  ) ;export
  (import (scheme base) (liii base) (liii list) (liii string) (liii unicode))
  (begin

    (define %paste-start-marker "\x1b;[200~")
    (define %paste-end-marker "\x1b;[201~")
    (define %paste-marker-len 6)
    (define %paste-max-lines 10)
    (define %paste-max-chars 1000)

    (define (%bracketed-paste? text)
      (and (string? text)
        (let ((len (string-length text)))
          (and (>= len 12)
            (string=? (substring text 0 %paste-marker-len) %paste-start-marker)
            (string=? (substring text (- len %paste-marker-len) len) %paste-end-marker)
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    (define (%bracketed-paste-content text)
      (if (%bracketed-paste? text)
        (let ((len (string-length text)))
          (substring text %paste-marker-len (- len %paste-marker-len))
        ) ;let
        ""
      ) ;if
    ) ;define

    (define (%count-newlines text)
      (let ((len (string-length text)))
        (let loop
          ((i 0) (n 0))
          (cond ((= i len) n)
                ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ n 1)))
                (else (loop (+ i 1) n))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%paste-lines text)
      (+ 1 (%count-newlines text))
    ) ;define

    ;; 归一化：CRLF/CR -> LF，Tab -> 4 空格，过滤除换行外的控制字符
    (define (%paste-normalize text)
      (let ((len (string-length text)) (p (open-output-string)))
        (let loop
          ((i 0) (start 0))
          (if (>= i len)
            (begin
              (when (> i start)
                (write-string (substring text start i) p)
              ) ;when
              (get-output-string p)
            ) ;begin
            (let ((c (string-ref text i)))
              (cond
               ((char=? c #\return)
                (when (> i start)
                  (write-string (substring text start i) p)
                ) ;when
                (write-string "\n" p)
                (if
                  (and (< (+ i 1) len) (char=? (string-ref text (+ i 1)) #\newline))
                  (loop (+ i 2) (+ i 2))
                  (loop (+ i 1) (+ i 1))
                ) ;if
               ) ;
               ((char=? c #\tab)
                (when (> i start)
                  (write-string (substring text start i) p)
                ) ;when
                (write-string "    " p)
                (loop (+ i 1) (+ i 1))
               ) ;
               ((and (char<? c #\space) (not (char=? c #\newline)))
                (when (> i start)
                  (write-string (substring text start i) p)
                ) ;when
                (loop (+ i 1) (+ i 1))
               ) ;
               (else (loop (+ i 1) start))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (%paste-large? text)
      (or (> (%paste-lines text) %paste-max-lines)
        (and (> (string-length text) %paste-max-chars)
          (> (utf8-string-length text) %paste-max-chars)
        ) ;and
      ) ;or
    ) ;define

    ;; >1000 字符时展示字符数，否则展示总行数
    (define (%paste-marker id text)
      (let ((chars (utf8-string-length text)) (lines (%paste-lines text)))
        (if (> chars %paste-max-chars)
          (string-append "[paste #"
            (number->string id)
            " "
            (number->string chars)
            " chars]"
          ) ;string-append
          (string-append "[paste #"
            (number->string id)
            " +"
            (number->string lines)
            " lines]"
          ) ;string-append
        ) ;if
      ) ;let
    ) ;define

    ;; ---------------------------------------------------------------
    ;; 注册表：entry = (marker . content)
    ;; ---------------------------------------------------------------

    (define-record-type <paste-registry>
      (%make-paste-registry entries counter)
      paste-registry?
      (entries %paste-entries %paste-entries-set!)
      (counter %paste-counter %paste-counter-set!)
    ) ;define-record-type

    (define (make-paste-registry)
      (%make-paste-registry '() 0)
    ) ;define

    (define (paste-registry-count reg)
      (%paste-counter reg)
    ) ;define

    (define (paste-registry-add! reg text)
      (let* ((id (+ (%paste-counter reg) 1)) (marker (%paste-marker id text)))
        (%paste-counter-set! reg id)
        (%paste-entries-set! reg (cons (cons marker text) (%paste-entries reg)))
        marker
      ) ;let*
    ) ;define

    (define (paste-registry-clear! reg)
      (%paste-entries-set! reg '())
      (%paste-counter-set! reg 0)
      #t
    ) ;define

    (define (paste-registry-remove! reg marker)
      (when (assoc marker (%paste-entries reg))
        (%paste-entries-set! reg
          (filter
            (lambda (entry) (not (string=? (car entry) marker)))
            (%paste-entries reg)
          ) ;filter
        ) ;%paste-entries-set!
        #t
      ) ;when
    ) ;define

    ;; 快照/恢复：entries 与 counter 均为不可变数据，撤销栈直接持引用
    (define (paste-registry-snapshot reg)
      (list (%paste-entries reg) (%paste-counter reg))
    ) ;define

    (define (paste-registry-restore! reg snap)
      (%paste-entries-set! reg (list-ref snap 0))
      (%paste-counter-set! reg (list-ref snap 1))
      #t
    ) ;define

    ;; text 的字节位置 i 起是否恰好是 marker（标记均为 ASCII，
    ;; UTF-8 多字节序列中不会出现 ASCII 字节，按字节比较安全）
    (define (%match-marker-at? text i len marker)
      (let ((m-len (string-length marker)))
        (and (<= (+ i m-len) len)
          (let loop
            ((j 0))
            (or (= j m-len)
              (and (char=? (string-ref text (+ i j)) (string-ref marker j)) (loop (+ j 1)))
            ) ;or
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    ;; pairs（(marker . content) 列表）中恰好从 i 开始的标记，返回该项或 #f
    (define (%lookup-marker pairs text i len)
      (let loop
        ((ps pairs))
        (if (null? ps)
          #f
          (if (%match-marker-at? text i len (caar ps)) (car ps) (loop (cdr ps)))
        ) ;if
      ) ;let
    ) ;define

    ;; 在 text 的 i 处展开标记：命中时把 [start, i) 段与标记内容
    ;; 写入 p，返回展开后的下一扫描位置；未命中返回 #f。
    (define (%expand-marker-at! pairs text i len start p)
      (let ((hit (%lookup-marker pairs text i len)))
        (and hit
          (let ((next (+ i (string-length (car hit)))))
            (when (> i start)
              (write-string (substring text start i) p)
            ) ;when
            (write-string (cdr hit) p)
            next
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    ;; 把文本中的标记展开为真实内容。
    ;; 单趟从左到右扫描：只有文本里原有的标记会被展开，替换进来的
    ;; 内容不再参与后续匹配，粘贴原文中恰好含有的旧标记文本保持
    ;; 字面，不会被二次展开。
    (define (paste-expand reg text)
      (let ((pairs (%paste-entries reg)))
        (if (null? pairs)
          text
          (let ((len (string-length text)) (p (open-output-string)))
            (let loop
              ((i 0) (start 0))
              (if (>= i len)
                (begin
                  (when (> i start)
                    (write-string (substring text start i) p)
                  ) ;when
                  (get-output-string p)
                ) ;begin
                ;; 标记都以 "[" 开头，其余字节直接跳过
                (if (char=? (string-ref text i) #\[)
                  (let ((next (%expand-marker-at! pairs text i len start p)))
                    (if next (loop next next) (loop (+ i 1) start))
                  ) ;let
                  (loop (+ i 1) start)
                ) ;if
              ) ;if
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 光标前是否恰好以一个有效标记结尾，返回 (marker . start-col) 或 #f
    (define (paste-marker-at-end reg line col)
      (let loop
        ((entries (%paste-entries reg)))
        (if (null? entries)
          #f
          (let* ((marker (caar entries)) (len (utf8-string-length marker)))
            (if
              (and (>= col len) (string=? (utf8-substring line (- col len) col) marker))
              (cons marker (- col len))
              (loop (cdr entries))
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 光标处是否以一个有效标记开头，返回 (marker . end-col) 或 #f
    (define (paste-marker-at-start reg line col)
      (let ((line-len (utf8-string-length line)))
        (let loop
          ((entries (%paste-entries reg)))
          (if (null? entries)
            #f
            (let* ((marker (caar entries)) (len (utf8-string-length marker)))
              (if
                (and (<= (+ col len) line-len)
                  (string=? (utf8-substring line col (+ col len)) marker)
                ) ;and
                (cons marker (+ col len))
                (loop (cdr entries))
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
