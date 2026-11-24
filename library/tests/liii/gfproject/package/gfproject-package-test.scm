;; gfproject-package
;; 构造 package 元数据记录。
;;
;; 语法
;; ----
;; (gfproject-package name organization [description])
;;
;; 参数
;; ----
;; name : string
;; 包名。
;;
;; organization : string
;; 发布组织。
;;
;; description : string（可选）
;; 包描述，默认为空字符串。
;;
;; 返回值
;; ------
;; gfproject-package?
;; 包含 name、organization 和 description 的包元数据记录。

(import (liii check) (liii gfproject package))

(check-set-mode! 'report-failed)

(let ((package (gfproject-package "lang" "liii" "Goldfish Scheme")))
  (check-true (gfproject-package? package))
  (check-false (gfproject-package? '(("name" . "lang"))))
  (check (gfproject-package-name package) => "lang")
  (check (gfproject-package-organization package) => "liii")
  (check (gfproject-package-description package) => "Goldfish Scheme")
  (check (gfproject-package-version package) => #f)
  (check (gfproject-package-id package) => "lang@liii")
) ;let

(let ((package (gfproject-package "pi" "liii")))
  (check (gfproject-package-description package) => "")
  (check (gfproject-package-version package) => #f)
) ;let

(let ((package (gfproject-package "pi" "liii" "Pi coding agent" "2026.11.24")))
  (check (gfproject-package-description package) => "Pi coding agent")
  (check (gfproject-package-version package) => "2026.11.24")
) ;let

(check-report)
