;; tool-card-set-started-at!
;; 设置工具卡片的起始计时时间戳。
;;
;; 语法
;; ----
;; (tool-card-set-started-at! card started-at)
;;
;; 参数
;; ----
;; card : tool-card-component?
;; 工具卡片组件。
;;
;; started-at : number? 或 #f
;; 新的起始时间戳（毫秒）或 #f。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未指定。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive views tool-card)
) ;import

(check-set-mode! 'report-failed)

(let ((card (make-tool-card "bash" '((command . "ls")) 'pending)))
  (tool-card-set-started-at! card 12345.0)
  (check (tool-card-started-at card) => 12345.0)
  (tool-card-set-started-at! card #f)
  (check-false (tool-card-started-at card))
) ;let

(check-report)
