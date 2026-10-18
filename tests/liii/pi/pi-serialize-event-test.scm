(import (liii check) (liii string) (liii pi))

(check-set-mode! 'report-failed)

;; pi-serialize-event
;; 将事件或消息 alist 序列化为单行 JSON 字符串。
(let ((json-str (pi-serialize-event '(("type" . "test") ("key" . "value")))))
  (check-true (string-contains? json-str "type")))

(check-report)
