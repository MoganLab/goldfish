;; thinking-card-set-hidden!
;; 设置思维链卡片的折叠/隐藏显示状态。
;;
;; 语法
;; ----
;; (thinking-card-set-hidden! card hidden)
;;
;; 参数
;; ----
;; card : component?
;; 思维链卡片组件。
;; hidden : boolean?
;; 是否隐藏具体思考内容。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive views thinking)
) ;import

(check-set-mode! 'report-failed)

;; 1. 隐藏后仅渲染 Thinking... 标签
(let* ((card (make-thinking-card "long detailed thoughts"))
       (lines1 (component-render card 80))
      ) ;
  (check-true (string-contains? (string-join lines1 "\n") "detailed"))
  (thinking-card-set-hidden! card #t)
  (let ((lines2 (component-render card 80)))
    (check-false (string-contains? (string-join lines2 "\n") "detailed"))
    (check-true (string-contains? (string-join lines2 "\n") "Thinking..."))
  ) ;let
) ;let*

(check-report)
