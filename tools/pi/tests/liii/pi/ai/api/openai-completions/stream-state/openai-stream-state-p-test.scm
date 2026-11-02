;; openai-stream-state?
;; 判断对象是否为 OpenAI 流状态记录。
;;
;; 语法
;; ----
;; (openai-stream-state? state)
;;
;; 参数
;; ----
;; state : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是流状态记录返回 #t，否则返回 #f。

(import (liii check)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 流状态记录
(check (openai-stream-state?
         (make-openai-stream-state '() #f '() "stop" #f '()))
  => #t)

;; 其它对象
(check (openai-stream-state? '()) => #f)
(check (openai-stream-state? #f) => #f)
(check (openai-stream-state? "state") => #f)
(check (openai-stream-state? 42) => #f)

(check-report)
