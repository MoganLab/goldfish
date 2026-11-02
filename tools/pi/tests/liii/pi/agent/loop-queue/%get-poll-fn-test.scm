;; %get-poll-fn
;; 从 Agent 配置 alist 中获取指定键的队列轮询回调过程。
;;
;; 语法
;; ----
;; (%get-poll-fn config key)
;;
;; 参数
;; ----
;; config : list?，包含配置项的 alist。
;; key : any?，配置项键名（如 'get-steering-messages）。
;;
;; 返回值
;; ----
;; (or procedure? #f)，若存在且为过程则返回该回调过程，否则返回 #f。

(import (scheme base) (liii check) (liii pi agent loop-queue))

(check-set-mode! 'report-failed)

;; 1. 存在回调过程
(let* ((poll (lambda () '("msg1"))) (cfg (list (cons 'get-steering-messages poll))))
  (check (%get-poll-fn cfg 'get-steering-messages) => poll)
) ;let*

;; 2. 键不存在返回 #f
(check (%get-poll-fn '((foo . 1)) 'get-steering-messages) => #f)

;; 3. 值不是过程返回 #f
(check (%get-poll-fn '((get-steering-messages . "not-a-fn")) 'get-steering-messages)
  =>
  #f
) ;check

;; 4. 非 pair 配置返回 #f
(check (%get-poll-fn '() 'get-steering-messages) => #f)
(check (%get-poll-fn #f 'get-steering-messages) => #f)

(check-report)
