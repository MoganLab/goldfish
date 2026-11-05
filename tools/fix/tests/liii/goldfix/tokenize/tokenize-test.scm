;; tokenize
;; 把源码切分为带 offset / end / line / column / text 信息的 token 列表，
;; 字符串、注释与字符字面量中的括号不作为结构括号。
;;
;; 语法
;; ----
;; (tokenize source)
;;
;; 参数
;; ----
;; source : string? 源码文本
;;
;; 返回值
;; ------
;; list? fix-token 记录列表

(import (liii check) (liii goldfix tokenize) (liii goldfix record))

(check-set-mode! 'report-failed)

;; 测试：基本的括号与记号切分
(let ((tokens (tokenize "(define x 1)")))
  (check (length tokens) => 5)
  (check (fix-token-type (car tokens)) => 'open-paren)
  (check (fix-token-column (car tokens)) => 0)
  (check (fix-token-type (cadr tokens)) => 'other)
  (check (fix-token-text (cadr tokens)) => "define")
  (check (fix-token-column (cadr tokens)) => 1)
  (check (fix-token-type (car (reverse tokens))) => 'close-paren)
) ;let

;; 测试：字符串、注释和字符字面量中的括号不作为结构括号。
(let ((tokens (tokenize "(display \")\") ; )\n#\\)")))
  (check (map fix-token-type tokens)
    =>
    '(open-paren other string close-paren line-comment char)
  ) ;check
  (check (fix-token-text (list-ref tokens 2)) => "\")\"")
  (check (fix-token-type (list-ref tokens 5)) => 'char)
) ;let

;; 测试：#\; 和 #\" 是字符字面量，不能被切成注释或字符串。
(let ((tokens (tokenize "(list #\\; #\\\") ; )")))
  (check (map fix-token-type tokens)
    =>
    '(open-paren other char char close-paren line-comment)
  ) ;check
  (check (fix-token-text (list-ref tokens 2)) => "#\\;")
  (check (fix-token-text (list-ref tokens 3)) => "#\\\"")
) ;let

;; 测试：raw string 中的括号不作为结构括号。
(let ((tokens (tokenize "(display #\"END\")END\")")))
  (check (map fix-token-type tokens)
    =>
    '(open-paren other raw-string close-paren)
  ) ;check
  (check (fix-token-text (list-ref tokens 2)) => "#\"END\")END\"")
) ;let

;; 测试：空 delimiter raw string 使用 "" 结束，结束时需要同时吃掉两个引号。
(let ((tokens (tokenize "(display #\"\"(not-code)\"\")")))
  (check (map fix-token-type tokens)
    =>
    '(open-paren other raw-string close-paren)
  ) ;check
  (check (fix-token-text (list-ref tokens 2)) => "#\"\"(not-code)\"\"")
) ;let

;; 测试：独立的 | 标识符不应被当作 bar symbol 的开始定界符。
(let ((tokens (tokenize "(or | a)")))
  (check (length tokens) => 5)
  (check (fix-token-type (list-ref tokens 2)) => 'other)
  (check (fix-token-text (list-ref tokens 2)) => "|")
  (check (fix-token-type (list-ref tokens 3)) => 'other)
  (check (fix-token-text (list-ref tokens 3)) => "a")
) ;let

(check-report)
