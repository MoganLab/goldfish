;; assistant-card-text
;; 获取助手正文卡片当前的文本内容。
;;
;; 语法
;; ----
;; (assistant-card-text card)
;;
;; 参数
;; ----
;; card : component?
;; 由 make-assistant-card 创建的助手正文卡片。
;;
;; 返回值
;; ----
;; string?
;; 卡片当前的文本内容。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive views assistant)
) ;import

(check-set-mode! 'report-failed)

(let ((card (make-assistant-card "初始文本")))
  (check (assistant-card-text card) => "初始文本")
  (assistant-card-append-text! card "追加")
  (check (assistant-card-text card) => "初始文本追加")
) ;let

(check-report)
