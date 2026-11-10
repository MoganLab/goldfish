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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(define-library (liii goldfmt tokenize)
  (export tokenize tokens->string escape-string-content)
  (import (liii base) (liii string) (liii list) (liii unicode))
  (begin

    ;; 行内空白字符：空格与制表符
    (define (inline-blank-char? c)
      (or (char=? c #\space) (char=? c #\tab))
    ) ;define

    ;; 行内空白行：空串或全部由空格/制表符构成
    (define (blank-line? str)
      (string-every inline-blank-char? str)
    ) ;define

    ;; 宽松空白行：换行、回车亦视作空白（EOF 收尾时行内可含换行符）
    (define (loose-blank-line? str)
      (string-every (lambda (c)
                      (or (inline-blank-char? c) (char=? c #\newline) (char=? c #\return))
                    ) ;lambda
        str
      ) ;string-every
    ) ;define

    ;; 行尾空白裁剪：纯空白行保持原样，其余去掉行尾空白
    (define (trim-right-spaces str)
      (if (blank-line? str) str (utf8-string-trim-right str))
    ) ;define

    (define (escape-string-content content)
      (string-join
        (map
          (lambda (c)
            (cond ((char=? c #\\) "\\\\")
                  ((char=? c #\") "\\\"")
                  ((char=? c #\newline) "\\n")
                  ((char=? c #\return) "")
                  (else (string c))
            ) ;cond
          ) ;lambda
          (string->list content)
        ) ;map
        ""
      ) ;string-join
    ) ;define

    (define (tokenize content)
      (let ((len (string-length content))
            (tokens '())
            (chunk-start 0)
            (line-chunks '())
            (in-string #f)
            (string-escaped #f)
            (in-block-comment #f)
            (in-raw-string #f)
            (raw-delimiter "")
            (raw-delimiter-match 0)
           ) ;
        ;; 取当前行内容：位置 chunk-start 至 end（不含），跳过块注释片段
        (define (get-current-line end)
          (if (null? line-chunks)
            (if (<= end chunk-start) "" (substring content chunk-start end))
            (let ((final-chunk (if (> end chunk-start) (list (substring content chunk-start end)) '())
                  ) ;final-chunk
                 ) ;
              (apply string-append (reverse (append final-chunk line-chunks)))
            ) ;let
          ) ;if
        ) ;define

        (define (reset-current-line! next-pos)
          (set! chunk-start next-pos)
          (set! line-chunks '())
        ) ;define

        ;; 从 pos 处继续扫描（先重置当前行累积状态）
        (define (continue-at! pos)
          (reset-current-line! pos)
          (process-char pos)
        ) ;define

        (define (char-literal-name-char? ch)
          (or (and (char>=? ch #\a) (char<=? ch #\z))
            (and (char>=? ch #\A) (char<=? ch #\Z))
            (and (char>=? ch #\0) (char<=? ch #\9))
          ) ;or
        ) ;define

        ;; pos 指向 #\ 之后的第一个字符；命名/十六进制字面量（#\space、#\x22）
        ;; 持续扫描到分隔符，单字符字面量（#\a、#\"、#\(）只跳过一个字符
        (define (char-literal-end pos)
          (if (and (< pos len) (char-literal-name-char? (string-ref content pos)))
            (let name-loop
              ((p (+ pos 1)))
              (if (and (< p len) (char-literal-name-char? (string-ref content p)))
                (name-loop (+ p 1))
                p
              ) ;if
            ) ;let
            (+ pos 1)
          ) ;if
        ) ;define

        ;; 代码行入栈：空白行不入栈
        (define (push-code-token! line)
          (unless (blank-line? line)
            (set! tokens (cons (cons 'code line) tokens))
          ) ;unless
        ) ;define

        ;; 行注释入栈：去掉行尾空白
        (define (push-comment-token! text)
          (set! tokens (cons (cons 'comment (trim-right-spaces text)) tokens))
        ) ;define

        ;; EOF 收尾：最后一行非空白则入栈，返回 token 序列
        (define (finish-at! i)
          (let ((line (get-current-line i)))
            (unless (loose-blank-line? line)
              (set! tokens (cons (cons 'code line) tokens))
            ) ;unless
            (reverse tokens)
          ) ;let
        ) ;define

        ;; 将 (chunk-start, end) 的代码片段压入当前行片段表（进入块注释前）
        (define (push-line-chunk! end)
          (when (> end chunk-start)
            (set! line-chunks (cons (substring content chunk-start end) line-chunks))
          ) ;when
        ) ;define

        ;; 进入块注释（i 指向 #| 的 #）
        (define (enter-block-comment! i)
          (push-line-chunk! i)
          (set! in-block-comment #t)
          (process-char (+ i 2))
        ) ;define

        ;; 离开块注释（i 指向 |# 的 |）：只推进 chunk-start，
        ;; 保留 line-chunks 以拼接注释前后的代码
        (define (leave-block-comment! i)
          (set! in-block-comment #f)
          (set! chunk-start (+ i 2))
          (process-char (+ i 2))
        ) ;define

        ;; 进入命名 delimiter 的 raw string（i 指向 #" 的 #）；
        ;; 无结束引号的孤立 #" 视作普通字符
        (define (enter-raw-string! i)
          (let ((delim-end (string-index content #\" (+ i 2))))
            (if delim-end
              (begin
                (set! in-raw-string #t)
                (set! raw-delimiter (substring content (+ i 2) delim-end))
                (set! raw-delimiter-match 0)
                (scan-raw-string! (+ delim-end 1))
              ) ;begin
              (scan-code! (+ i 1))
            ) ;if
          ) ;let
        ) ;define

        ;; 离开 raw string，next 为跳过结束符后的扫描位置
        (define (leave-raw-string! next)
          (set! in-raw-string #f)
          (set! raw-delimiter "")
          (set! raw-delimiter-match 0)
          (process-char next)
        ) ;define

        ;; 双引号字符串内部扫描（含转义）
        (define (scan-string! i)
          (if (>= i len)
            (finish-at! i)
            (let ((c (string-ref content i)))
              (cond (string-escaped (set! string-escaped #f) (scan-string! (+ i 1)))
                    ((char=? c #\\) (set! string-escaped #t) (scan-string! (+ i 1)))
                    ((char=? c #\") (set! in-string #f) (scan-code! (+ i 1)))
                    (else (scan-string! (+ i 1)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;define

        ;; 块注释内部扫描：只认 |# 结束符
        (define (scan-block-comment! i)
          (if (>= i len)
            (finish-at! i)
            (let ((c (string-ref content i))
                  (next-c (if (< (+ i 1) len) (string-ref content (+ i 1)) #\nul))
                 ) ;
              (if (and (char=? c #\|) (char=? next-c #\#))
                (leave-block-comment! i)
                (scan-block-comment! (+ i 1))
              ) ;if
            ) ;let
          ) ;if
        ) ;define

        ;; raw string 内部扫描：逐字符匹配命名 delimiter，
        ;; 空 delimiter 以连续两个引号结束
        (define (scan-raw-string! i)
          (if (>= i len)
            (finish-at! i)
            (let ((c (string-ref content i))
                  (next-c (if (< (+ i 1) len) (string-ref content (+ i 1)) #\nul))
                 ) ;
              (cond ((and (string-null? raw-delimiter) (char=? c #\") (char=? next-c #\"))
                     (leave-raw-string! (+ i 2))
                    ) ;
                    ((and (< raw-delimiter-match (string-length raw-delimiter))
                       (char=? c (string-ref raw-delimiter raw-delimiter-match))
                     ) ;and
                     (set! raw-delimiter-match (+ raw-delimiter-match 1))
                     (if (and (= raw-delimiter-match (string-length raw-delimiter)) (char=? next-c #\"))
                       ;; delimiter 完整出现且后随引号：结束
                       (leave-raw-string! (+ i 2))
                       (scan-raw-string! (+ i 1))
                     ) ;if
                    ) ;
                    (else (set! raw-delimiter-match 0) (scan-raw-string! (+ i 1)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;define

        ;; 行注释（i 指向第一个分号）：注释前代码入栈，注释内容去行尾空白
        (define (emit-line-comment! i)
          (push-code-token! (get-current-line i))
          (let* ((newline-pos (string-index content #\newline i))
                 (comment-end (or newline-pos len))
                ) ;
            (push-comment-token! (substring content (+ i 2) comment-end))
            (if newline-pos (continue-at! (+ newline-pos 1)) (reverse tokens))
          ) ;let*
        ) ;define

        ;; 统计连续空行：pos 为扫描起点，blank-count 为已累计空行数
        (define (count-blank-lines! pos blank-count)
          (cond ((>= pos len) (reverse tokens))
                ((char=? (string-ref content pos) #\return)
                 (count-blank-lines! (+ pos 1) blank-count)
                ) ;
                ((char=? (string-ref content pos) #\newline)
                 (count-blank-lines! (+ pos 1) (+ blank-count 1))
                ) ;
                (else
                  (when (> blank-count 0)
                    (set! tokens (cons (cons 'newline blank-count) tokens))
                  ) ;when
                  (continue-at! pos)
                ) ;else
          ) ;cond
        ) ;define

        ;; 换行处理（i 指向换行符）：非空白行入栈；空白行与后续空行
        ;; 合并为 newline token，文件开头的空白行静默丢弃
        (define (emit-newline! i)
          (let ((line (get-current-line i)))
            (if (blank-line? line)
              (if (null? tokens) (continue-at! (+ i 1)) (count-blank-lines! (+ i 1) 1))
              (begin
                (set! tokens (cons (cons 'code line) tokens))
                (continue-at! (+ i 1))
              ) ;begin
            ) ;if
          ) ;let
        ) ;define

        ;; 代码状态扫描（顶层词法状态）
        (define (scan-code! i)
          (if (>= i len)
            (finish-at! i)
            (let ((c (string-ref content i))
                  (next-c (if (< (+ i 1) len) (string-ref content (+ i 1)) #\nul))
                 ) ;
              (cond ((and (char=? c #\#) (char=? next-c #\|)) (enter-block-comment! i))
                    ((and (char=? c #\#) (char=? next-c #\\))
                     ;; 字符字面量 #\X：整体跳过，避免其中的 " ; 被误判
                     (scan-code! (char-literal-end (+ i 2)))
                    ) ;
                    ((and (char=? c #\#) (char=? next-c #\")) (enter-raw-string! i))
                    ((char=? c #\") (set! in-string #t) (scan-string! (+ i 1)))
                    ((and (char=? c #\;) (char=? next-c #\;)) (emit-line-comment! i))
                    ((char=? c #\newline) (emit-newline! i))
                    (else (scan-code! (+ i 1)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;define

        ;; 状态分派：按当前词法状态进入对应扫描器
        (define (process-char i)
          (cond (in-string (scan-string! i))
                (in-block-comment (scan-block-comment! i))
                (in-raw-string (scan-raw-string! i))
                (else (scan-code! i))
          ) ;cond
        ) ;define

        (process-char 0)
      ) ;let
    ) ;define

    (define (tokens->string tokens)
      (string-join
        (map
          (lambda (token)
            (let ((type (car token)) (content (cdr token)))
              (cond ((eq? type 'comment)
                     (string-append "(*comment* \"" (escape-string-content content) "\")")
                    ) ;
                    ((eq? type 'newline) (string-append "(*newline* " (number->string content) ")"))
                    (else content)
              ) ;cond
            ) ;let
          ) ;lambda
          tokens
        ) ;map
        "\n"
      ) ;string-join
    ) ;define
  ) ;begin
) ;define-library
