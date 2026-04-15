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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldfmt-tokenize)
  (export tokenize tokens->string escape-string-content)
  (import (liii base)
          (liii string)
          (liii unicode)
  ) ;import
  
  (begin
    ;;; 辅助函数：安全地移除字符串尾部空格
    ;;; 使用 utf8-string-trim-right 正确处理 Unicode 字符
    ;;; 如果内容只有空格，则保留原样
    (define (trim-right-spaces str)
      (if (string-every (lambda (c) (or (char=? c #\space) (char=? c #\tab))) str)
          str
          (utf8-string-trim-right str)
      ) ;if
    ) ;define
    
    ;;; 辅助函数：将注释内容转义为可在 Scheme 字符串中使用的形式
    ;;; 处理双引号和反斜杠
    (define (escape-string-content content)
      (let loop ((chars (string->list content))
                 (result '()))
        (if (null? chars)
            (list->string (reverse result))
            (let ((c (car chars)))
              (cond
                ((char=? c #\\) (loop (cdr chars) (cons #\\ (cons #\\ result))))
                ((char=? c #\") (loop (cdr chars) (cons #\" (cons #\\ result))))
                ((char=? c #\newline) (loop (cdr chars) (cons #\n (cons #\\ result))))
                ((char=? c #\return) (loop (cdr chars) result))
                (else (loop (cdr chars) (cons c result)))
              ) ;cond
            ) ;let
        ) ;if
      ) ;let
    ) ;define
    
    ;;; 辅助函数：检查字符串是否为空或只包含空白字符
    (define (blank-line? str)
      (or (string-null? str)
          (string-every (lambda (c) (or (char=? c #\space) (char=? c #\tab))) str)))

    ;;; Tokenize 函数：将内容分解为 token 列表
    ;;; 正确处理跨行字符串、跨行注释和普通字符串中的 ;;
    ;;; 返回 (token-type . content) 列表
    ;;; 新增：识别连续换行作为空行 (newline . n)
    (define (tokenize content)
      (let ((len (string-length content))
            (tokens '())
            (current-line "")
            (in-string #f)
            (string-escaped #f)
            (in-block-comment #f)
            (in-raw-string #f)
            (raw-delimiter "")
            (raw-delimiter-match 0)
      ) ;let

      (define (process-char i)
        (if (>= i len)
            (begin
              (if (and (not (string-null? current-line))
                       (not (string-every (lambda (c) (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline) (char=? c #\return))) current-line)))
                   (set! tokens (cons (cons 'code current-line) tokens))
              ) ;if
              (reverse tokens)
            ) ;begin
            (let ((c (string-ref content i))
                  (next-c (if (< (+ i 1) len) (string-ref content (+ i 1)) #\nul)))
              (cond
                (in-string
                  (set! current-line (string-append current-line (string c)))
                  (cond
                    (string-escaped (set! string-escaped #f) (process-char (+ i 1)))
                    ((char=? c #\\) (set! string-escaped #t) (process-char (+ i 1)))
                    ((char=? c #\") (set! in-string #f) (process-char (+ i 1)))
                    (else (process-char (+ i 1)))
                  ) ;cond
                ) ;in-string

                (in-block-comment
                  (cond
                     ((and (char=? c #\|) (char=? next-c #\#))
                      (set! in-block-comment #f)
                      (process-char (+ i 2))
  ;
 ;
                     ) ;
                    (else
                     (process-char (+ i 1))
                    ) ;else
                  ) ;cond
                ) ;in-block-comment

                (in-raw-string
                  (set! current-line (string-append current-line (string c)))
                  (cond
                    ((and (< raw-delimiter-match (string-length raw-delimiter))
                          (char=? c (string-ref raw-delimiter raw-delimiter-match))
                     ) ;and
                     (set! raw-delimiter-match (+ raw-delimiter-match 1))
                     (if (>= raw-delimiter-match (string-length raw-delimiter))
                         (if (and (< (+ i 1) len)
                                  (char=? (string-ref content (+ i 1)) #\"))
                             (begin
                               (set! current-line (string-append current-line "\""))
                               (set! in-raw-string #f)
                               (set! raw-delimiter "")
                               (set! raw-delimiter-match 0)
                               (process-char (+ i 2))
                             ) ;begin
                             (process-char (+ i 1))
                         ) ;if
                         (process-char (+ i 1))
                     ) ;if
  ;
 ;
                    ) ;
                    ((char=? c #\newline)
                     (set! raw-delimiter-match 0)
                     (process-char (+ i 1))
  ;
 ;
                    ) ;
                    (else
                     (set! raw-delimiter-match 0)
                     (process-char (+ i 1))
                    ) ;else
                  ) ;cond
                ) ;in-raw-string

                (else
                  (cond
                    ((and (char=? c #\#) (char=? next-c #\|))
                     (set! in-block-comment #t)
                     (process-char (+ i 2))
  ;
 ;
                    ) ;

                    ((and (char=? c #\#) (char=? next-c #\"))
                     (let ((delim-end (string-index content #\" (+ i 2))))
                       (if delim-end
                           (begin
                             (set! in-raw-string #t)
                             (set! raw-delimiter (substring content (+ i 2) delim-end))
                             (set! raw-delimiter-match 0)
                             (set! current-line
                                   (string-append current-line "#\"" raw-delimiter "\"")
                             ) ;set!
                             (process-char (+ delim-end 1))
                           ) ;begin
                           (begin
                             (set! current-line (string-append current-line (string c)))
                             (process-char (+ i 1))
                           ) ;begin
                       ) ;if
                     ) ;let
  ;
 ;
                    ) ;

                    ((char=? c #\")
                     (set! in-string #t)
                     (set! current-line (string-append current-line (string c)))
                     (process-char (+ i 1))
  ;
 ;
                    ) ;

                      ((and (char=? c #\;) (char=? next-c #\;))
                         (if (and (not (string-null? current-line))
                                  (not (string-every (lambda (c) (or (char=? c #\space) (char=? c #\tab))) current-line)))
                             (set! tokens (cons (cons 'code current-line) tokens))
                         ) ;if
                         (let*
                           ((comment-start (+ i 2))
                            (newline-pos (string-index content #\newline i))
                            (comment-end (or newline-pos len))
                            (raw-content (substring content comment-start comment-end))
                            (trimmed-content (trim-right-spaces raw-content))
                           ) ;let* bindings
                           (set! tokens (cons (cons 'comment trimmed-content) tokens))
                           (if newline-pos
                               (begin
                                 (set! current-line "")
                                 (process-char (+ newline-pos 1))
                               ) ;begin
                               (reverse tokens)
                           ) ;if
                         ) ;let*
                      ) ;

                      ((char=? c #\newline)
                       ; 检查 current-line 是否有非空白内容
                       (if (and (not (string-null? current-line))
                                (not (blank-line? current-line)))
                           ; 有代码内容，保存代码
                           (begin
                             (set! tokens (cons (cons 'code current-line) tokens))
                             (set! current-line "")
                             (process-char (+ i 1)))
                           ; current-line为空或只有空白
                           ; 只有在已经有其他token时才记录空行（排除文件开头的空白）
                           (if (null? tokens)
                               ; 文件开头，跳过这些空白
                               (begin
                                 (set! current-line "")
                                 (process-char (+ i 1)))
                               ; 已经有token，统计连续空行
                               (let count-loop ((pos (+ i 1))
                                                (blank-count (if (blank-line? current-line) 1 0)))
                                  (if (>= pos len)
                                      ; 到达文件末尾 - 不保留末尾空行
                                      (reverse tokens)
                                     (let ((next-char (string-ref content pos)))
                                       (cond
                                         ; 跳过回车符
                                         ((char=? next-char #\return)
                                          (count-loop (+ pos 1) blank-count))
                                         ; 遇到换行符，继续统计
                                         ((char=? next-char #\newline)
                                          (count-loop (+ pos 1) (+ blank-count 1)))
                                         ; 遇到其他字符，停止统计
                                         (else
                                          (when (> blank-count 0)
                                            (set! tokens (cons (cons 'newline blank-count) tokens)))
                                          (set! current-line "")
                                          (process-char pos))))))))
                      ) ;

                     (else
                      (set! current-line (string-append current-line (string c)))
                      (process-char (+ i 1))
                     ) ;else
                  ) ;cond
                ) ;else
              ) ;cond
            ) ;let
        ) ;if
      ) ;define

      (process-char 0))
    ) ;define

    ;;; 辅助函数：将 token 列表重新组装为可读取的字符串
    (define (tokens->string tokens)
      (string-join
        (map
          (lambda (token)
            (let ((type (car token))
                  (content (cdr token)))
              (cond
                ((eq? type 'comment)
                 (string-append "(*comment* \""
                                (escape-string-content content)
                                "\")"
                 ) ;string-append
 ;
                ) ;
                ((eq? type 'newline)
                 (string-append "(*newline* "
                                (number->string content)
                                ")"
                 ) ;string-append
 ;
                ) ;
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
