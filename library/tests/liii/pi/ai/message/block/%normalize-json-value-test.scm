;; %normalize-json-value
;; 递归规范化消息内容为标准 Scheme-JSON 值。
;;
;; 语法
;; ----
;; (%normalize-json-value obj)
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

(import (liii check) (liii pi ai message block) (liii pi ai message text-block))
(check-set-mode! 'report-failed)
(check (%normalize-json-value (list (pi-text-block "hi")))
  =>
  #((("type" . "text") ("text" . "hi")))
) ;check

(check-report)
