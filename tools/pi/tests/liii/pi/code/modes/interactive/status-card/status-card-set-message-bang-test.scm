;; status-card-set-message!
;; 原地更新状态提示卡片的消息文本。
;;
;; 语法
;; ----
;; (status-card-set-message! card msg)
;;
;; 参数
;; ----
;; card : status-card?
;; 状态提示卡片组件。
;; msg : string?
;; 新的状态提示消息。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive status-card)
) ;import

(check-set-mode! 'report-failed)

;; 1. 更新卡片文本后重新渲染体现新内容
(let* ((card (make-status-card "Initial message"))
       (lines1 (component-render card 80))
      ) ;
  (check-true (string-contains? (list-ref lines1 1) "Initial message"))
  (status-card-set-message! card "Updated message")
  (let ((lines2 (component-render card 80)))
    (check-true (string-contains? (list-ref lines2 1) "Updated message"))
    (check-false (string-contains? (list-ref lines2 1) "Initial message"))
  ) ;let
) ;let*

(check-report)
