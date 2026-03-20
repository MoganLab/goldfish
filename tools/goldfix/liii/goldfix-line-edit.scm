;;; Goldfix Line Edit 模块
;;; 行级右括号编辑函数
;;;
;;; Copyright (c) 2024 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-line-edit)
  (import (scheme base))
  (import (liii error))
  (import (liii string))
  (import (liii goldfix-constant))
  (import (liii goldfix-line-scan))

  (export line-starts-with-rparen?)
  (export line-trailing-countable-rparen-count)
  (export line-trailing-unmatched-rparen-count)
  (export line-has-only-trailing-rparens?)
  (export line-is-formatted-right-tag?)
  (export remove-rparens-from-right)
  (export remove-rparens-from-chars)

  (begin
    ;; 检查行是否以 ) 开头（忽略前导空格）
    (define (line-starts-with-rparen? line)
      (let ((trimmed (string-trim line)))
        (and (not (string-null? trimmed))
             (char=? (string-ref trimmed 0) RPAREN)
        ) ;and
      ) ;let
    ) ;define

    ;; 检查行是否是格式正确的右标记行（) ; tag）
    ;; 这样的行应该被保留，即使它不在 collect-rparen-lines 中
    ;; 输入: line - 行字符串
    ;; 输出: #t 如果是格式正确的右标记行
    (define (line-is-formatted-right-tag? line)
      (let ((trimmed (string-trim line)))
        (and (not (string-null? trimmed))
             (char=? (string-ref trimmed 0) RPAREN)
             (let ((len (string-length trimmed)))
               (let loop ((i 1))
                 (cond
                   ((>= i len) #f)
                   ((char=? (string-ref trimmed i) #\;)
                    (let ((after-semicolon (string-trim (substring trimmed (+ i 1) len))))
                      (not (string-null? after-semicolon))
                    ) ;let
                   ) ;
                   (else
                    (loop (+ i 1))
                   ) ;else
                 ) ;cond
               ) ;let
             ) ;let
        ) ;and
      ) ;let
    ) ;define

    ;; 统计一行末尾由可计数右括号构成的纯尾随后缀长度。
    ;; 允许尾部有空白或行注释；一旦遇到其它代码，就停止计数。
    (define (line-trailing-countable-rparen-count line)
      (count-trailing-rparen-indices line (countable-rparen-indices line))
    ) ;define

    ;; 统计一行末尾真正“净闭合”掉外层环境的右括号数量。
    ;; 只有这些在本行内找不到配对左括号的 )，才可能被安全提升成独立右标记行。
    (define (line-trailing-unmatched-rparen-count line)
      (count-trailing-rparen-indices line (unmatched-rparen-indices line))
    ) ;define

    ;; 检查一行里的可计数右括号是否全部构成“尾随右括号后缀”
    ;; 也就是：从第一个可计数右括号开始，到代码结束位置之间，只允许出现空白和右括号。
    (define (line-has-only-trailing-rparens? line)
      (let ((indices (countable-rparen-indices line)))
        (and (not (null? indices))
             (= (length indices)
                (line-trailing-countable-rparen-count line)
             ) ;=
        ) ;and
      ) ;let
    ) ;define

    (define (mark-removal-range! flags line idx)
      (vector-set! flags idx #t)
      (let loop ((j (- idx 1)))
        (when (and (>= j 0)
                   (char-whitespace? (string-ref line j))
                   (not (vector-ref flags j)))
          (vector-set! flags j #t)
          (loop (- j 1))
        ) ;when
      ) ;let
    ) ;define

    (define (line-without-flagged-chars line flags)
      (let ((len (string-length line)))
        (let loop ((i 0) (chars '()))
          (if (>= i len)
            (list->string (reverse chars))
            (if (vector-ref flags i)
              (loop (+ i 1) chars)
              (loop (+ i 1) (cons (string-ref line i) chars))
            ) ;if
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 从右侧移除指定数量的右括号
    ;; 输入: chars - 反转的字符列表, count - 需要移除的数量
    ;; 输出: 移除后的字符串
    ;; 文档详见 tests/liii/goldfix-line-test.scm
    (define (remove-rparens-from-chars chars count)
      (let ((line (list->string (reverse chars))))
        (if (<= count 0)
          line
          (let* ((indices (countable-rparen-indices line))
                 (total (length indices)))
            (if (< total count)
              (value-error "(liii goldfix-line) remove-rparens-from-right: Not enough right parens to remove")
              (let* ((targets (list-tail indices (- total count)))
                     (flags (make-vector (string-length line) #f)))
                (for-each (lambda (idx)
                            (mark-removal-range! flags line idx))
                          (reverse targets)
                ) ;for-each
                (line-without-flagged-chars line flags)
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 从行右侧移除指定数量的右括号（跳过字符串和注释）
    ;; 输入: line - 行字符串, count - 要移除的数量
    ;; 输出: 移除后的行字符串
    ;; 错误: 当没有足够的右括号可移除时抛出 value-error
    ;; 文档详见 tests/liii/goldfix-line-test.scm
    (define (remove-rparens-from-right line count)
      (remove-rparens-from-chars (reverse (string->list line)) count)
    ) ;define

  ) ;begin
) ;define-library
