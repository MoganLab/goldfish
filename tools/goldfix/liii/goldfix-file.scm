(define-library (liii goldfix-file)
  (import (scheme base))
  (import (liii string))
  (import (liii error))

  ;; ---------- 导出接口 ----------
  (export read-file-lines)
  (export string->lines)
  (export lines->string)

  (begin

    ;; 读取文件为行列表
    ;; 输入: file-path - 文件路径
    ;;       start-line - 起始行号（可选，1-based，默认为1）
    ;;       end-line - 结束行号（可选，1-based，默认为文件末尾）
    ;; 输出: 行列表（每行不含换行符）
    (define (read-file-lines file-path . args)
      (let ((start-line (if (null? args) 1 (car args)))
            (end-line (if (or (null? args) (null? (cdr args)))
                          +inf.0
                          (cadr args)))
            ) ;end-line
        (call-with-input-file file-path
          (lambda (port)
            (let loop ((lines '())
                       (current-line 1)
                       (line (read-line port)))
              (cond
                ;; 文件结束或超出范围
                ((or (eof-object? line) (> current-line end-line))
                 (reverse lines)
                ) ;
                ;; 在范围内，收集行
                ((>= current-line start-line)
                 (loop (cons line lines)
                       (+ current-line 1)
                       (read-line port)
                 ) ;loop
                ) ;
                ;; 还没到起始行，跳过
                (else
                 (loop lines
                       (+ current-line 1)
                       (read-line port)
                 ) ;loop
                ) ;else
              ) ;cond
            ) ;let
          ) ;lambda
        ) ;call-with-input-file
      ) ;let
    ) ;define

    ;; 将字符串分割为行列表
    ;; 输入: str - 字符串
    ;; 输出: 行列表（每行不含换行符）
    (define (string->lines str)
      (if (string-null? str)
        '()
        (string-split str #\newline)
      ) ;if
    ) ;define

    ;; 将行列表合并为字符串
    ;; 输入: lines - 行列表
    ;; 输出: 合并后的字符串
    (define (lines->string lines)
      (string-join lines "\n")
    ) ;define

  ) ;begin
) ;define-library
