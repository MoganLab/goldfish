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

(define-library (liii goldfmt-scan)
  (export scan scan-string scan-file)
  (import (liii base)
          (liii path)
          (liii string)
          (liii list)
          (liii goldfmt-record)
  ) ;import
  
  (begin
    ;;; atom? 辅助函数：判断是否为 Scheme 原子类型
    ;;; 原子类型包括：symbol, number, string, boolean, char, 空列表, vector, eof-object, 以及其他不可解析的对象
    (define (atom? x)
      (or (symbol? x)
          (number? x)
          (string? x)
          (boolean? x)
          (char? x)
          (null? x)
          (vector? x)
          (eof-object? x)
          (eq? 'undefined? (type-of x))
          (unspecified? x)
      ) ;or
    ) ;define
    
    ;;; scan 函数：将 s-exp 扫描成 env 或 atom
    ;;; 输入 datum: s-exp（可以是 atom 或 list）
    ;;; 返回: env 记录或 atom 记录
    (define (scan datum)
      (scan-datum datum 0)
    ) ;define
    
    ;;; 辅助函数：扫描 datum，带深度参数
    ;;; depth: 当前深度（根节点为 0）
    (define (scan-datum datum depth)
      (if (atom? datum)
          ;;; 如果是 atom，创建 atom 记录
          ;; left-line 和 right-line 使用默认值 0
          (make-atom :depth depth :value datum)
          ;;; 如果是 list，需要遍历创建 env
          (scan-list datum depth)
      ) ;if
    ) ;define
    
    ;;; 辅助函数：判断是否为有效的 tag-name
    ;;; symbol 或 syntax（如 #_quote, #_quasiquote 等）都可以作为 tag
    (define (tag? x)
      (or (symbol? x)
          (and (syntax? x) #t)
      ) ;or
    ) ;define
    
    ;;; 将 tag 转换为字符串
    (define (tag->string x)
      (if (symbol? x)
          (symbol->string x)
          (object->string x #f)
      ) ;if
    ) ;define
    
    ;;; 辅助函数：判断是否为 quote 形式：(quote x) 或 (#_quote x)
    (define (quote-form? lst)
      (and (pair? lst)
           (not (null? lst))
           (or (eq? (car lst) 'quote)
               (and (syntax? (car lst))
                    (string=? (object->string (car lst) #f) "#_quote")
               ) ;and
           ) ;or
           (not (null? (cdr lst)))
      ) ;and
    ) ;define

    ;;; 辅助函数：将点对列表转换为普通列表，保留 . 符号
    ;;; 例如：(x y . rest) -> (x y . rest)
    ;;; 返回一个可以被 map 和 list->vector 正确处理的列表
    (define (dotted-list-elements lst)
      (let loop ((current lst)
                 (result '()))
        (cond
          ; 如果 current 是 pair，继续递归
          ((pair? current)
           (loop (cdr current) (cons (car current) result))
 ;
          ) ;
          ; 如果 current 是 null，返回反转后的结果
          ((null? current)
           (reverse result)
 ;
          ) ;
          ; 否则是点对尾部，添加 . 和尾部元素
          (else
            (reverse (cons current (cons (string->symbol ".") result)))
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;;; 辅助函数：扫描列表或点对，创建 env 结构
    ;;; depth: 当前深度（根节点为 0）
    ;;; 注意：空列表 '() 是 atom，不会进入此函数
    ;;; 支持点对 (a . b) 的处理
    (define (scan-list lst depth)
      (let* ((first (car lst))
             (rest (cdr lst)))
        ;;; 处理 quote 形式：整个作为一个 env，没有 children
        (if (quote-form? lst)
            (make-env :tag-name (if (syntax? first) "#_quote" (symbol->string first))
                      :depth depth
                      :children (vector)
                      :value lst
            ) ;make-env
            ;;; 处理点对：使用 dotted-list? 检测点对
            ;;; 点对的第一元素不作为 tag，整个列表作为无 tag env
            (let
              ((children-list
                 (if (dotted-list? lst)
                     ; 点对情况：转换为包含 . 符号的普通列表
                     (dotted-list-elements lst)
                     ; 正常列表情况
                     (if (tag? first) rest lst)
                 ) ;if
               ) ;children-list
               (has-tag? (and (not (dotted-list? lst)) (tag? first)))
 ;
              ) ;
              (let*
                ((scanned-children
                   (map
                     (lambda (child)
                      (scan-datum child (+ depth 1))
                     ) ;lambda
                     children-list
                   ) ;map
                 ) ;scanned-children
                 (children-vec (list->vector scanned-children))
 ;
                ) ;
                (if has-tag?
                    (make-env :tag-name (tag->string first)
                              :depth depth
                              :children children-vec
                              :value lst
                    ) ;make-env
                    (make-env :tag-name ""
                              :depth depth
                              :children children-vec
                              :value lst
                    ) ;make-env
                ) ;if
              ) ;let*
            ) ;let
        ) ;if
      ) ;let*
    ) ;define
    
    ;;; scan-string 函数：从字符串扫描所有顶层表达式
    ;;; 输入 str: Scheme 代码字符串（例如："(+ 1 2) (+ 3 4)"）
    ;;; 返回: 扫描后的 env/atom 记录列表（vector）
    (define (scan-string str)
      (let ((port (open-input-string str)))
        (let loop ((results '()))
          (let ((datum (read port)))
            (if (eof-object? datum)
                (list->vector (reverse results))
                (loop (cons (scan-datum datum 0) results))
            ) ;if
          ) ;let
        ) ;let
      ) ;let
    ) ;define
    
    ;;; 辅助函数：检查字符是否为空白字符
    (define (whitespace-char? c)
      (or (char=? c #\space)
          (char=? c #\tab)
          (char=? c #\newline)
          (char=? c #\return)
      ) ;or
    ) ;define
    
    ;;; 辅助函数：检查字符串在位置 pos 是否处于字符串字面量中
    ;;; 返回 #t 如果在字符串中，#f 否则
    (define (inside-string? str pos)
      (let loop ((i 0)
                 (in-string #f)
                 (escaped #f))
        (cond
          ((>= i pos) in-string)
          ((>= i (string-length str)) in-string)
          (else
            (let ((c (string-ref str i)))
              (cond
                (escaped (loop (+ i 1) in-string #f))
                ((char=? c #\\) (loop (+ i 1) in-string #t))
                ((char=? c #\") (loop (+ i 1) (not in-string) #f))
                (else (loop (+ i 1) in-string #f))
              ) ;cond
            ) ;let
          ) ;else
        ) ;cond
      ) ;let
    ) ;define
    
    ;;; 辅助函数：检查字符串在位置 pos 是否处于跨行字符串中
    ;;; 跨行字符串使用 #"..." 语法
    (define (inside-raw-string? str pos)
      (let loop ((i 0)
                 (in-raw-string #f)
                 (raw-delimiter ""))
        (cond
          ((>= i pos) in-raw-string)
          ((>= i (string-length str)) in-raw-string)
          ((and (< (+ i 1) (string-length str))
                (char=? (string-ref str i) #\#)
                (char=? (string-ref str (+ i 1)) #\")
           ) ;and
           ; 开始跨行字符串
           (let ((end-pos (string-index str #\" (+ i 2))))
             (if end-pos
                 (let ((delimiter (substring str (+ i 2) end-pos)))
                   (loop (+ end-pos 1) #t delimiter)
                 ) ;let
                 (loop (+ i 2) #t "")
             ) ;if
           ) ;let
 ;
          ) ;
          (in-raw-string
            ; 检查是否遇到结束 delimiter
            (let ((del-len (string-length raw-delimiter)))
              (if (and (<= (+ i del-len) (string-length str))
                       (string=? (substring str i (+ i del-len)) raw-delimiter)
                       (< (+ i del-len) (string-length str))
                       (char=? (string-ref str (+ i del-len)) #\"))
                (loop (+ i del-len 1) #f "")
                (loop (+ i 1) #t raw-delimiter)
              ) ;if
            ) ;let
          ) ;in-raw-string
          (else (loop (+ i 1) #f ""))
        ) ;cond
      ) ;let
    ) ;define
    
    ;;; 辅助函数：查找真正的注释开始位置（不考虑字符串中的 ;;）
    ;;; 返回注释开始的索引，如果没有找到则返回 #f
    (define (find-real-comment-start line)
      (let loop ((pos 0))
        (cond
          ((>= pos (- (string-length line) 1)) #f)
          ((and (char=? (string-ref line pos) #\;)
                (char=? (string-ref line (+ pos 1)) #\;)
                (not (inside-string? line pos))
                (not (inside-raw-string? line pos))
           ) ;and
           pos
 ;
          ) ;
          (else (loop (+ pos 1)))
        ) ;cond
      ) ;let
    ) ;define
    
    ;;; 辅助函数：检查一行是否为真正的注释行
    ;;; 格式: [空白] ;; [注释内容]
    ;;; 返回: 如果是注释行返回注释开始位置，否则返回 #f
    (define (comment-line? line)
      (let ((comment-pos (find-real-comment-start line)))
        (if comment-pos
            ; 检查 ;; 前面是否只有空白字符
            (let ((prefix (substring line 0 comment-pos)))
              (if (or (string-null? prefix)
                      (string-every whitespace-char? prefix))
                  comment-pos
                  #f
              ) ;if
            ) ;let
            #f
        ) ;if
      ) ;let
    ) ;define
    
    ;;; 辅助函数：提取注释内容
    ;;; 输入: 一行文本和注释开始位置
    ;;; 返回: 注释内容（不含 ;; 前缀）
    (define (extract-comment-content line comment-pos)
      (string-trim-both 
        (substring line (+ comment-pos 2))
      ) ;string-trim-both
    ) ;define
    
    ;;; 辅助函数：将注释内容转义为可在字符串中使用的形式
    ;;; 处理双引号和反斜杠
    (define (escape-comment-content content)
      (let loop ((chars (string->list content))
                 (result '()))
        (if (null? chars)
            (list->string (reverse result))
            (let ((c (car chars)))
              (cond
                ((char=? c #\\) (loop (cdr chars) (cons #\\ (cons #\\ result))))
                ((char=? c #\") (loop (cdr chars) (cons #\" (cons #\\ result))))
                (else (loop (cdr chars) (cons c result)))
              ) ;cond
            ) ;let
        ) ;if
      ) ;let
    ) ;define
    
    ;;; Tokenize 函数：将文件内容分解为 token 列表
    ;;; 每个 token 是 (类型 . 内容) 对，类型可以是 'code 或 'comment
    (define (tokenize content)
      (let* ((lines (string-split content #\newline))
             (tokens '()))
        (for-each 
          (lambda (line)
            (let ((comment-pos (comment-line? line)))
              (if comment-pos
                  ; 是注释行
                  (let ((content (extract-comment-content line comment-pos)))
                    (set! tokens (cons (cons 'comment content) tokens))
                  ) ;let
                  ; 是普通代码
                  (set! tokens (cons (cons 'code line) tokens))
              ) ;if
            ) ;let
          ) ;lambda
          lines
        ) ;for-each
        (reverse tokens)
      ) ;let*
    ) ;define
    
    ;;; 辅助函数：将 token 列表重新组装为可读取的字符串
    ;;; 将注释转换为 (*comment* "内容") 形式
    (define (tokens->string tokens)
      (string-join
        (map
          (lambda (token)
            (let ((type (car token))
                  (content (cdr token)))
              (if (eq? type 'comment)
                  (string-append "(*comment* \"" 
                                 (escape-comment-content content)
                                 "\")"
                  ) ;string-append
                  content
              ) ;if
            ) ;let
          ) ;lambda
          tokens
        ) ;map
        "\n"
      ) ;string-join
    ) ;define
    
    ;;; scan-file 函数：从文件读取并扫描所有顶层表达式
    ;;; 输入 path: 文件路径字符串
    ;;; 返回: 扫描后的 env/atom 记录列表（vector）
    (define (scan-file path)
      (let* ((raw-content (path-read-text path))
             (tokens (tokenize raw-content))
             (processed-content (tokens->string tokens)))
        (scan-string processed-content)
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
