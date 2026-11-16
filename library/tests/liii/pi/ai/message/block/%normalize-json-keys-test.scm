;; %normalize-json-keys
;; 将 JSON 对象的顶层符号键归一化为字符串键。
;;
;; 语法
;; ----
;; (%normalize-json-keys obj)
;;
;; 参数
;; ----
;; obj : any?
;; 输入对象。
;;
;; 返回值
;; ----
;; any?
;; 转换后的结果。

(import (liii check) (liii pi ai message block))
(check-set-mode! 'report-failed)
(check (%normalize-json-keys '((type . "text") (text . "hi")))
  =>
  '(("type" . "text") ("text" . "hi"))
) ;check

(check-report)
