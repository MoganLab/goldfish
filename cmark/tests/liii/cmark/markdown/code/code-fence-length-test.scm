;; code-fence-length
;; 计算围栏代码块的安全 fence 长度。
;;
;; 语法
;; ----
;; (code-fence-length literal)
;;
;; 参数
;; ----
;; literal : string
;; 代码块字面内容。
;;
;; 返回值
;; ----
;; integer
;; 长度为 max(3, 内容最长反引号游程 + 1) 的 fence 反引号数。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown code))

(check-set-mode! 'report-failed)

(check-true (procedure? code-fence-length))

(check (code-fence-length "abc") => 3)
(check (code-fence-length "") => 3)
(check (code-fence-length "```\nx") => 4)
(check (code-fence-length "x```") => 4)
(check (code-fence-length "a````b") => 5)

(check-report)
