;; line-start-close?
;; 判断 token 是否为行首的右括号：token 必须是右括号，
;; 且与所在行的第一枚 code token 处于同一偏移量。
;;
;; 语法
;; ----
;; (line-start-close? token line)
;;
;; 参数
;; ----
;; token : any? 待判断的 token
;; line : any? token 所在的 fix-line 记录
;;
;; 返回值
;; ------
;; boolean? 是行首右括号时为 #t，否则为 #f

(import (liii check) (liii goldfix tokenize) (liii goldfix record))

(check-set-mode! 'report-failed)

;; 测试：单独成行的右括号是行首右括号
(let* ((lines (tokenize-lines "(a\n)"))
       (first-line (car lines))
       (second-line (cadr lines))
      ) ;
  (check (line-start-close? (fix-line-first-code-token second-line) second-line)
    =>
    #t
  ) ;check
  (check (line-start-close? (fix-line-first-code-token first-line) first-line)
    =>
    #f
  ) ;check
) ;let*

;; 测试：同一行中不在行首的右括号不是行首右括号
(let* ((lines (tokenize-lines "(a )"))
       (line (car lines))
       (tokens (fix-line-tokens line))
      ) ;
  (check (line-start-close? (fix-line-first-code-token line) line) => #f)
  (check (line-start-close? (car (reverse tokens)) line) => #f)
) ;let*

;; 测试：参数不是 token 或 fix-line 时返回 #f
(check (line-start-close? 'a 'b) => #f)

(check-report)
