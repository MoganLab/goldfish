
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark block parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-fenced-code-block
;; 解析围栏式代码块。
;;
;; 语法
;; ----
;; (parse-fenced-code-block lines)
;;
;; 描述
;; ----
;; fenced-code-start? 判断起始围栏，
;; fenced-code-block-end? 判断终止围栏。

(check-true (fenced-code-start? "```scheme"))
(check-true (fenced-code-start? " ```"))
(check-false (fenced-code-start? "    ```"))
(check-true (fenced-code-block-end? #\` 3 "```"))
(check-true (fenced-code-block-end? #\~ 3 "~~~"))
(check-true (fenced-code-block-end? #\` 3 "````"))
(check-true (fenced-code-block-end? #\~ 4 "~~~~~"))
(check-false (fenced-code-block-end? #\` 3 "``"))
(check-false (fenced-code-block-end? #\~ 4 "~~~"))
(check-false (fenced-code-block-end? #\` 3 "``` aaa"))
(check-false (fenced-code-block-end? #\` 3 "    ```"))

(let ((code-block (parse-fenced-code-block '("```scheme" "(define x 1)" "```"))))
  (check (cmark-node-type code-block) => 'code_block)
  (check (cdr (assoc 'info (cmark-node-data code-block))) => "scheme")
  (check (cdr (assoc 'literal (cmark-node-data code-block))) => "(define x 1)")
) ;let

(let ((code-block (parse-fenced-code-block '("~~~" "code" "~~~"))))
  (check (cmark-node-type code-block) => 'code_block)
  (check (cdr (assoc 'info (cmark-node-data code-block))) => "")
  (check (cdr (assoc 'literal (cmark-node-data code-block))) => "code")
) ;let

(let ((code-block (parse-fenced-code-block '("```python" "print('hello')"
                                             "print('world')" "```"))
      ) ;code-block
     ) ;
  (check (cmark-node-type code-block) => 'code_block)
  (check (cdr (assoc 'info (cmark-node-data code-block))) => "python")
  (check (cdr (assoc 'literal (cmark-node-data code-block)))
    =>
    "print('hello')\nprint('world')"
  ) ;check
) ;let

(check-report)
