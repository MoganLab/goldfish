;; %convert-message-content
;; 将消息内容中的 JSON 块对象转换为 content block record。
;;
;; 语法
;; ----
;; (%convert-message-content obj)
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
(let ((res (%convert-message-content (vector '(("type" . "text")
                                               ("text" . "hi"))))))
  (check-true (vector? res))
  (check-true (pi-text-block? (vector-ref res 0)))
) ;let

(check-report)
