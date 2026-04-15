(import (liii check)
        (liii goldfix-tokenize)
        (liii goldfix-record)
        (liii raw-string))

(check-set-mode! 'report-failed)

;; tokenize
;; 返回带 offset / line / column 的 token 列表。

(let ((tokens (tokenize "(define x 1)")))
  (check (length tokens) => 5)
  (check (fix-token-type (car tokens)) => 'open-paren)
  (check (fix-token-column (car tokens)) => 0)
  (check (fix-token-type (cadr tokens)) => 'other)
  (check (fix-token-text (cadr tokens)) => "define")
  (check (fix-token-column (cadr tokens)) => 1)
  (check (fix-token-type (car (reverse tokens))) => 'close-paren))

;; 字符串、注释和字符字面量中的括号不作为结构括号。

(let ((tokens (tokenize "(display \")\") ; )\n#\\)")))
  (check (map fix-token-type tokens)
         => '(open-paren other string close-paren line-comment char))
  (check (fix-token-text (list-ref tokens 2)) => "\")\"")
  (check (fix-token-type (list-ref tokens 5)) => 'char))

;; #\; 和 #\" 是字符字面量，不能被切成注释或字符串。

(let ((tokens (tokenize "(list #\\; #\\\") ; )")))
  (check (map fix-token-type tokens)
         => '(open-paren other char char close-paren line-comment))
  (check (fix-token-text (list-ref tokens 2)) => "#\\;")
  (check (fix-token-text (list-ref tokens 3)) => "#\\\"")
)
;; raw string 中的括号不作为结构括号。

(let ((tokens (tokenize "(display #\"END\")END\")")))
  (check (map fix-token-type tokens)
         => '(open-paren other raw-string close-paren))
  (check (fix-token-text (list-ref tokens 2)) => "#\"END\")END\"")
)
;; 空 delimiter raw string 使用 "" 结束，结束时需要同时吃掉两个引号。

(let ((tokens (tokenize "(display #\"\"(not-code)\"\")")))
  (check (map fix-token-type tokens)
         => '(open-paren other raw-string close-paren))
  (check (fix-token-text (list-ref tokens 2)) => "#\"\"(not-code)\"\""))

;; tokenize-lines
;; 按物理行组织 token，并记录第一枚 code token。

(let ((lines (tokenize-lines (&- #""
                                (begin
                                  (display "x")
                                ) ;begin
                                "")
                          ;&-
             )))
  (check (length lines) => 3)
  (check (fix-token-column (fix-line-first-code-token (car lines))) => 0)
  (check (fix-token-column (fix-line-first-code-token (cadr lines))) => 2)
  (check (fix-token-column (fix-line-first-code-token (caddr lines))) => 0)
  (check (line-start-close?
           (fix-line-first-code-token (caddr lines))
           (caddr lines))
         => #t))

(check-report)
