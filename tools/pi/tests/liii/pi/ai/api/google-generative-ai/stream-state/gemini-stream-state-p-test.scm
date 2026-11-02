;; gemini-stream-state?
;; 判断对象是否为 Gemini 流状态记录。
;;
;; 语法
;; ----
;; (gemini-stream-state? state)
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
  (liii pi ai api google-generative-ai stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 流状态记录
(check (gemini-stream-state?
         (make-gemini-stream-state '() #f '() '() "STOP" #f "" ""))
  => #t)

;; 其它对象
(check (gemini-stream-state? '()) => #f)
(check (gemini-stream-state? #f) => #f)
(check (gemini-stream-state? "state") => #f)
(check (gemini-stream-state? 42) => #f)

(check-report)
