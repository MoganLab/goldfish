(import (liii check)
  (liii pi code cli args)
) ;import

(check-set-mode! 'report-failed)

;; --session-dir 参数解析

;; 1. 默认未指定为 #f
(check (pi-args-ref (parse-pi-args '()) 'session-dir) => #f)

;; 2. 空格传参
(check (pi-args-ref (parse-pi-args '("--session-dir" "/tmp/my-sessions"))
         'session-dir)
  => "/tmp/my-sessions")

;; 3. 不吞掉后续位置参数
(let ((parsed (parse-pi-args '("--session-dir" "/tmp/s" "hello"))))
  (check (pi-args-ref parsed 'session-dir) => "/tmp/s")
  (check (pi-args-ref parsed 'messages) => '("hello")))

;; 4. 位于 -- 之后时作为普通消息，不触发选项解析
(let ((parsed (parse-pi-args '("--" "--session-dir"))))
  (check (pi-args-ref parsed 'session-dir) => #f)
  (check (pi-args-ref parsed 'messages) => '("--session-dir")))

(check-report)
