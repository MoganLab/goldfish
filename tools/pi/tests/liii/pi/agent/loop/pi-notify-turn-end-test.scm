;; pi-notify-turn-end
;; 派发单轮对话结束事件 turn_end，携带助理响应消息与该轮工具执行结果。
;;
;; 语法
;; ----
;; (pi-notify-turn-end emit assistant-msg tool-results)
;;
;; 参数
;; ----
;; emit : (or procedure? #f)，事件派发回调过程。
;; assistant-msg : any?，本轮模型返回的 assistant 响应消息对象。
;; tool-results : list?，本轮工具执行得到的结果消息列表。
;;
;; 返回值
;; ----
;; unspecified，若 emit 为过程则触发 turn_end 事件，否则无操作。

(import (scheme base) (liii check) (liii pi agent loop))

(check-set-mode! 'report-failed)

;; 1. 正常派发 turn_end 事件
(let* ((evs '())
       (mock-emit
         (lambda (ev-type . args) (set! evs (cons (cons ev-type args) evs)))
       ) ;mock-emit
       (msg '((role . "assistant") (content . "done")))
       (results '(((role . "toolResult"))))
      ) ;
  (pi-notify-turn-end mock-emit msg results)
  (check (length evs) => 1)
  (check (caar evs) => 'turn_end)
  (check (cadar evs) => msg)
  (check (caddar evs) => results)
) ;let*

;; 2. emit 为 #f 时无操作不报错
(check-true (begin (pi-notify-turn-end #f '() '()) #t))

(check-report)
