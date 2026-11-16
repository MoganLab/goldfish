;; assistant-card-component?
;; 判断对象是否为助手正文卡片组件。
;;
;; 语法
;; ----
;; (assistant-card-component? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检测的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 若为助手正文卡片组件则返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive views assistant)
  (liii pi code modes interactive views thinking)
) ;import

(check-set-mode! 'report-failed)

(let ((card (make-assistant-card "hi")) (thinking (make-thinking-card "think")))
  (check (assistant-card-component? card) => #t)
  (check (assistant-card-component? thinking) => #f)
  (check (assistant-card-component? "hi") => #f)
) ;let

(check-report)
