;; format-rule-line
;; 格式化单条规则检查结果的行字符串（包含状态、名称与耗时）。
;;
;; 语法
;; ----
;; (format-rule-line ok name elapsed)
;;
;; 参数
;; ----
;; ok : boolean      规则是否通过
;; name : string     规则名称
;; elapsed : number  执行耗时（秒）
;;
;; 返回值
;; ------
;; string
;; 格式化后的结果行字符串。

(import (scheme base) (liii check) (liii string) (liii goldcheck report))

(check-set-mode! 'report-failed)

;; 1. 规则通过时的行格式
(let ((line (format-rule-line #t "max-file-lines" 0.1234)))
  (check-true (string-contains? line "[OK    ]"))
  (check-true (string-contains? line "max-file-lines"))
  (check-true (string-contains? line "0.123s"))
) ;let

;; 2. 规则失败时的行格式
(let ((line (format-rule-line #f "no-crlf" 1.5)))
  (check-true (string-contains? line "[FAILED]"))
  (check-true (string-contains? line "no-crlf"))
  (check-true (string-contains? line "1.500s"))
) ;let

(check-report)
