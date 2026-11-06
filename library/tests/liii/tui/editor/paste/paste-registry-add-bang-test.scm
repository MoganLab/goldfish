;; paste-registry-add!
;; 向粘贴注册表登记大段文本并生成折叠标记占位符。
;;
;; 语法
;; ----
;; (paste-registry-add! reg text)
;;
;; 参数
;; ----
;; reg : paste-registry?
;; 粘贴注册表实例。
;;
;; text : string?
;; 待注册的大段文本。
;;
;; 返回值
;; ----
;; string?
;; 形如 "[paste #N M chars]" 或 "[paste #N +M lines]" 的标记占位符。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(let ((reg (make-paste-registry)))
  (check (paste-registry-add! reg (make-string 1965 #\a))
    =>
    "[paste #1 1965 chars]"
  ) ;check
  (check (paste-registry-add! reg (string-append "a" (make-string 11 #\newline) "b"))
    =>
    "[paste #2 +12 lines]"
  ) ;check
  (check (paste-registry-count reg) => 2)
) ;let

(check-report)
