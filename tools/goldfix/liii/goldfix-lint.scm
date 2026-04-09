(define-library (liii goldfix-lint)
  (import (scheme base))
  (import (liii error))
  (import (liii string))
  (import (liii ascii))
  (import (liii goldfix-file))
  (import (liii goldfix-scheme))
  (export paren-match?
          line-balanced?
          file-balanced?
          check-lines-balanced
          count-parens-with-state
          count-parens
  ) ;export

  (begin
    (define (count-parens-with-state line block-depth in-string escape-next)
      ;; 检查输入是否包含换行符
      (when (string-index line #\newline)
        (value-error "(liii goldfix-lint) count-parens: input line must not contain newline character")
      ) ;when
      (let ((len (string-length line)))
        (let loop ((i 0)
                   (lparen-count 0)
                   (rparen-count 0)
                   (block-depth block-depth)
                   (in-string in-string)
                   (escape-next escape-next))
          (if (>= i len)
            (let-values (((next-block-depth next-in-string next-escape-next)
                          (advance-lex-state-at-line-break block-depth
                                                           in-string
                                                           escape-next))
                          ) ;advance-lex-state-at-line-break
              (values (cons lparen-count rparen-count)
                      next-block-depth
                      next-in-string
                      next-escape-next
              ) ;values
            ) ;let-values
            (let ((ch (string-ref line i)))
              (let-values (((next-i next-block-depth next-in-string next-escape-next mode)
                            (advance-lex-state line
                                               i
                                               block-depth
                                               in-string
                                               escape-next))
                            ) ;advance-lex-state
                (cond
                  ((eq? mode 'line-comment)
                   (let-values (((after-block-depth after-in-string after-escape-next)
                                 (advance-lex-state-at-line-break next-block-depth
                                                                  next-in-string
                                                                  next-escape-next))
                                 ) ;advance-lex-state-at-line-break
                     (values (cons lparen-count rparen-count)
                             after-block-depth
                             after-in-string
                             after-escape-next
                     ) ;values
                   ) ;let-values
                  ) ;
                  ((eq? mode 'code-char)
                   (loop next-i
                         (if (ascii-left-paren? ch)
                           (+ lparen-count 1)
                           lparen-count
                         ) ;if
                         (if (ascii-right-paren? ch)
                           (+ rparen-count 1)
                           rparen-count
                         ) ;if
                         next-block-depth
                         next-in-string
                         next-escape-next
                   ) ;loop
                  ) ;
                  (else
                   (loop next-i
                         lparen-count
                         rparen-count
                         next-block-depth
                         next-in-string
                         next-escape-next
                   ) ;loop
                  ) ;else
                ) ;cond
              ) ;let-values
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (count-parens line)
      (let-values (((counts _block-depth _in-string _escape-next)
                    (count-parens-with-state line 0 #f #f)))
        counts
      ) ;let-values
    ) ;define

    ;; 检查行列表中的括号是否匹配
    ;; 返回: #t 或 #f
    (define (check-lines-balanced lines)
      (let loop ((remaining lines)
                 (balance 0)
                 (block-depth 0)
                 (in-string #f)
                 (escape-next #f))
        (if (null? remaining)
          ;; 所有行处理完毕，检查 balance 是否为 0
          (= balance 0)
          (let-values (((paren-counts next-block-depth next-in-string next-escape-next)
                        (count-parens-with-state (car remaining)
                                                 block-depth
                                                 in-string
                                                 escape-next))
                        ) ;count-parens-with-state
            (let* ((lparen-count (car paren-counts))
                   (rparen-count (cdr paren-counts))
                   (new-balance (+ balance lparen-count (- rparen-count))))
              ;; balance < 0 表示右括号过多，提前返回 #f
              (if (< new-balance 0)
                #f
                (loop (cdr remaining)
                      new-balance
                      next-block-depth
                      next-in-string
                      next-escape-next
                ) ;loop
              ) ;if
            ) ;let*
          ) ;let-values
        ) ;if
      ) ;let
    ) ;define

    ;; 检查文件指定行范围的括号匹配
    ;; file-path: 文件路径
    ;; start-line: 起始行号(从1开始)
    ;; end-line: 结束行号(包含)
    ;; 返回: #t 或 #f
    ;; 错误: 当 end-line < start-line 或 start-line < 1 时抛出 value-error
    (define (paren-match? file-path start-line end-line)
      ;; 参数校验
      (when (< end-line start-line)
        (value-error "(liii goldfix-lint) paren-match?: end-line must be >= start-line, got start-line=~A, end-line=~A"
                     start-line end-line
        ) ;value-error
      ) ;when
      (when (< start-line 1)
        (value-error "(liii goldfix-lint) paren-match?: start-line must be >= 1, got ~A" start-line)
      ) ;when
      ;; 按文件真实上下文扫描到 end-line，确保起始状态正确。
      (let ((lines (read-file-lines file-path)))
        (let loop ((remaining lines)
                   (line-num 1)
                   (balance 0)
                   (range-start-balance #f)
                   (block-depth 0)
                   (in-string #f)
                   (escape-next #f))
          (cond
            ((null? remaining)
             #f
            ) ;
            (else
             (let
               ((current-range-start-balance
                 (if (= line-num start-line)
                   balance
                   range-start-balance
                 ) ;if
                ) ;current-range-start-balance
               ) ;
               (let-values (((paren-counts next-block-depth next-in-string next-escape-next)
                             (count-parens-with-state (car remaining)
                                                      block-depth
                                                      in-string
                                                      escape-next))
                             ) ;count-parens-with-state
                 (let* ((lparen-count (car paren-counts))
                        (rparen-count (cdr paren-counts))
                        (new-balance (+ balance lparen-count (- rparen-count))))
                   (cond
                     ((< new-balance 0)
                      #f
                     ) ;
                     ((= line-num end-line)
                      (and current-range-start-balance
                           (= new-balance current-range-start-balance)
                      ) ;and
                     ) ;
                     (else
                      (loop (cdr remaining)
                            (+ line-num 1)
                            new-balance
                            current-range-start-balance
                            next-block-depth
                            next-in-string
                            next-escape-next
                      ) ;loop
                     ) ;else
                   ) ;cond
                 ) ;let*
               ) ;let-values
             ) ;let
            ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    

    ;; 检查整个文件括号匹配
    (define (file-balanced? file-path)
      ;; 读取所有行并检查
      (call-with-input-file file-path
        (lambda (port)
          (let loop ((lines '())
                     (line (read-line port)))
            (if (eof-object? line)
              (check-lines-balanced (reverse lines))
              (loop (cons line lines) (read-line port))
            ) ;if
          ) ;let
        ) ;lambda
      ) ;call-with-input-file
    ) ;define

    

    ;; line-balanced?
    ;; 检查指定行的括号是否匹配
    ;;
    ;; 语法
    ;; ----
    ;; (line-balanced? file-path line-num)
    ;;
    ;; 参数
    ;; ----
    ;; file-path : string
    ;;   文件路径字符串
    ;; line-num : integer
    ;;   行号（1-based）
    ;;
    ;; 返回值
    ;; ----
    ;; boolean
    ;;   该行括号匹配返回 #t，不匹配返回 #f
    ;;   字符串和注释中的括号会被正确忽略
    ;;
    ;; 错误
    ;; ----
    ;; value-error
    ;;   当 line-num < 1 时抛出
    (define (line-balanced? file-path line-num)
      (paren-match? file-path line-num line-num)
    ) ;define
  ) ;begin
) ;define-library
