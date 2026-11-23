;; get-package-id-part
;; 校验 name@organization 的单个组件，字符集与 package 命名一致。
;;
;; 语法
;; ----
;; (get-package-id-part value)
;;
;; 参数
;; ----
;; value : string?
;; 待校验组件。
;;
;; 返回值
;; ----
;; string
;; 合法时原样返回；非法时抛 value-error。

(import (liii check) (liii gfproject get source))

(check-set-mode! 'report-failed)

(check (get-package-id-part "highlight") => "highlight")
(check (get-package-id-part "liii") => "liii")
(check (get-package-id-part "my-pkg_2") => "my-pkg_2")

(for-each
  (lambda (s) (check-catch 'value-error (get-package-id-part s)))
  '("" "Highlight" "1pkg" "-pkg" "a@b" "a/b" "a b")
) ;for-each

(check-report)
