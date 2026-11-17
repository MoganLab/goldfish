(import (liii check) (liii cmark block parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-indented-code-block
;; 解析缩进式代码块。
;;
;; 语法
;; ----
;; (parse-indented-code-block lines)
;;
;; 描述
;; ----
;; indented-code-line? 用于识别以四个空格或一个 tab 开头的代码行。

(check-true (indented-code-line? "    code"))
(check-true (indented-code-line? "\tcode"))
(check-false (indented-code-line? "  code"))

;; 缩进已在 collect-indented-code-block-lines 剥离，本函数直接拼接
(let ((code-block (parse-indented-code-block '("code 1" "code 2"))))
  (check (cmark-node-type code-block) => 'code_block)
  (check (cdr (assoc 'literal (cmark-node-data code-block))) => "code 1\ncode 2")
) ;let

(check-report)
