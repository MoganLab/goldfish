(import (liii check)
  (liii goldfix-tokenize)
  (liii goldfix-record)
  (liii raw-string)
) ;import

(check-set-mode! 'report-failed)

;; tokenize
;; 返回带 offset / line / column 的 token 列表。

(let ((tokens (tokenize "(define x 1)")))
  (check (length tokens) => 5)
  (check (fix-token-type (car tokens))
    =>
    'open-paren
  ) ;check
  (check (fix-token-column (car tokens))
    =>
    0
  ) ;check
  (check (fix-token-type (cadr tokens))
    =>
    'other
  ) ;check
  (check (fix-token-text (cadr tokens))
    =>
    "define"
  ) ;check
  (check (fix-token-column (cadr tokens))
    =>
    1
  ) ;check
  (check (fix-token-type (car (reverse tokens)))
    =>
    'close-paren
  ) ;check
) ;let

;; 字符串、注释和字符字面量中的括号不作为结构括号。

(let ((tokens (tokenize "(display \")\") ; )\n#\\)")
      ) ;tokens
     ) ;
  (check (map fix-token-type tokens)
    =>
    '(open-paren other string close-paren line-comment char)
  ) ;check
  (check (fix-token-text (list-ref tokens 2))
    =>
    "\")\""
  ) ;check
  (check (fix-token-type (list-ref tokens 5))
    =>
    'char
  ) ;check
) ;let

;; #\; 和 #\" 是字符字面量，不能被切成注释或字符串。

(let ((tokens (tokenize "(list #\\; #\\\") ; )")
      ) ;tokens
     ) ;
  (check (map fix-token-type tokens)
    =>
    '(open-paren other char char close-paren line-comment)
  ) ;check
  (check (fix-token-text (list-ref tokens 2))
    =>
    "#\\;"
  ) ;check
  (check (fix-token-text (list-ref tokens 3))
    =>
    "#\\\""
  ) ;check
) ;let
;; raw string 中的括号不作为结构括号。

(let ((tokens (tokenize "(display #\"END\")END\")")
      ) ;tokens
     ) ;
  (check (map fix-token-type tokens)
    =>
    '(open-paren other raw-string close-paren)
  ) ;check
  (check (fix-token-text (list-ref tokens 2))
    =>
    "#\"END\")END\""
  ) ;check
) ;let
;; 空 delimiter raw string 使用 "" 结束，结束时需要同时吃掉两个引号。

(let ((tokens (tokenize "(display #\"\"(not-code)\"\")"
              ) ;tokenize
      ) ;tokens
     ) ;
  (check (map fix-token-type tokens)
    =>
    '(open-paren other raw-string close-paren)
  ) ;check
  (check (fix-token-text (list-ref tokens 2))
    =>
    "#\"\"(not-code)\"\""
  ) ;check
) ;let

;; tokenize-lines
;; 按物理行组织 token，并记录第一枚 code token。

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
  (check (fix-token-column (fix-line-first-code-token (car lines))
         ) ;fix-token-column
    =>
    0
  ) ;check
  (check (fix-token-column (fix-line-first-code-token (cadr lines))
         ) ;fix-token-column
    =>
    2
  ) ;check
  (check (fix-token-column (fix-line-first-code-token (caddr lines)
                           ) ;fix-line-first-code-token
         ) ;fix-token-column
    =>
    0
  ) ;check
  (check (line-start-close? (fix-line-first-code-token (caddr lines)
                            ) ;fix-line-first-code-token
           (caddr lines)
         ) ;line-start-close?
    =>
    #t
  ) ;check
) ;let

(check-report)
