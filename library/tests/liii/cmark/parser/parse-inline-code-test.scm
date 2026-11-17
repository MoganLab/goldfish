(import (liii check) (liii cmark inline parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-inline-code
;; 解析以反引号包裹的行内代码。
;;
;; 语法
;; ----
;; (parse-inline-code str start)
;;
;; 描述
;; ----
;; inline-code-start? 用于判断当前位置是否为行内代码入口。

(check-true (inline-code-start? "`code`" 0))
(check-false (inline-code-start? "`code`" 1))
(check-false (inline-code-start? "text" 0))

(let ((result (parse-inline-code "`hello`" 0)))
  (check (cmark-node-type (car result)) => 'code)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "hello"
  ) ;check
  (check (cdr result) => 7)
) ;let

;; 未闭合 code span 只消费起始反引号，不吞剩余（CommonMark Example 14）
(let ((result (parse-inline-code "`unclosed" 0)))
  (check (cmark-node-type (car result)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car result))))
    =>
    "`"
  ) ;check
  (check (cdr result) => 1)
) ;let

(let ((result (parse-inline-code "``a ` b``" 0)))
  (check (cmark-node-type (car result)) => 'code)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "a ` b"
  ) ;check
  (check (cdr result) => 9)
) ;let

(check-report)
