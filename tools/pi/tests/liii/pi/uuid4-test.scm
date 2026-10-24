(import (liii check) (liii uuid))

(check-set-mode! 'report-failed)

;; uuid4
;; 生成随机 UUIDv4 格式字符串。
(let ((id (uuid4)))
  (check-true (string? id))
  (check (string-length id) => 36))

(check-report)
