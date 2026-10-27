(import (liii check) (liii pi code modes output))

(check-set-mode! 'report-failed)

;; pi-last-assistant-text
;; 从消息列表末尾向前查找，返回最后一条非空白文本内容。
;;
;; 语法
;; ----
;; (pi-last-assistant-text messages) -> string
;;
;; 参数
;; ----
;; messages : list
;; 消息或可转换为消息 alist 的对象列表，文本从 content 字段提取。
;;
;; 返回值
;; ----
;; string
;; 最后一条非空白文本；空列表或所有文本均为空白时返回空字符串。
;; 查找会在命中后立即停止。
;;
;; 示例
;; ----
;; (pi-last-assistant-text messages) => "最后一条有效回复"

(define (text-message text)
  (list
    (cons 'content (vector (list (cons 'type "text") (cons 'text text))))
  ) ;list
) ;define

(check (pi-last-assistant-text '()) => "")
(check (pi-last-assistant-text (list (text-message "first") (text-message "last")))
  =>
  "last"
) ;check
(check (pi-last-assistant-text (list (text-message "last") (text-message "  \n")))
  =>
  "last"
) ;check

(check-report)
