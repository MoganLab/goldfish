;; pi-normalize-session-msg
;; 规范化会话消息条目，将 JSON 向量形式的 content 转为列表。
;;
;; 语法
;; ----
;; (pi-normalize-session-msg msg)
;;
;; 参数
;; ----
;; msg : any
;; 会话消息 alist。
;;
;; 返回值
;; ----
;; any
;; 规范化后的会话消息。

(import (liii check) (liii pi code core session-manager normalize))

(check-set-mode! 'report-failed)

;; 1. content 为向量时转为列表
(let ((msg (list (cons "content" (vector "a" "b")))))
  (check (pi-normalize-session-msg msg) => (list (cons "content" (list "a" "b"))))
) ;let

;; 2. content 已是列表时保持不变
(let ((msg (list (cons "content" (list "a")))))
  (check (pi-normalize-session-msg msg) => msg)
) ;let

;; 3. 非 alist 输入原样返回
(check (pi-normalize-session-msg "not-a-message") => "not-a-message")

(check-report)
