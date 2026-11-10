;; tokenize-lines
;; 按物理行组织 token，并记录每行的第一枚 code token 与行首偏移量。
;;
;; 语法
;; ----
;; (tokenize-lines source)
;;
;; 参数
;; ----
;; source : string? 源码文本
;;
;; 返回值
;; ------
;; list? fix-line 记录列表，每个物理行一个元素

(import (liii check)
  (liii goldfix tokenize)
  (liii goldfix record)
  (liii raw-string)
) ;import

(check-set-mode! 'report-failed)

;; 测试：按物理行分组，并记录每行首个 code token 的列号
(let ((lines (tokenize-lines (&- #""
                                (begin
                                  (display "x")
                                ) ;begin
                                ""
                             ) ;&-
             ) ;tokenize-lines
      ) ;lines
     ) ;
  (check (length lines) => 3)
  (check (fix-token-column (fix-line-first-code-token (car lines))) => 0)
  (check (fix-token-column (fix-line-first-code-token (cadr lines))) => 2)
  (check (fix-token-column (fix-line-first-code-token (caddr lines))) => 0)
  (check (line-start-close? (fix-line-first-code-token (caddr lines)) (caddr lines))
    =>
    #t
  ) ;check
) ;let

;; 测试：行号与行首偏移量按物理行递增
(let ((lines (tokenize-lines "a\nbb\n")))
  (check (map fix-line-number lines) => '(1 2 3))
  (check (map fix-line-start-offset lines) => '(0 2 5))
) ;let

(check-report)
