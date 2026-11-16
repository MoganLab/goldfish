;; pi-agent-event-normalize-json
;; 将 Scheme 值归一化为可序列化的 JSON 结构。
;;
;; 语法
;; ----
;; (pi-agent-event-normalize-json x)
;;
;; 参数
;; ----
;; x : any
;; 任意 Scheme 值。
;;
;; 返回值
;; ----
;; any
;; 归一化后的 JSON 结构。

(import (liii check) (liii pi agent event))

(check-set-mode! 'report-failed)

(check (pi-agent-event-normalize-json '()) => #())
(check (pi-agent-event-normalize-json '((a . 1))) => '(("a" . 1)))

(check-report)
