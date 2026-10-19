(import (liii check)
        (liii pi)
        (scheme time))

(check-set-mode! 'report-failed)

;; pi-event-session
;; 构造 session header 事件对象。
(let ((ev (pi-event-session "/tmp/cwd")))
  (check-true (pi-event-session? ev))
  (check (pi-event-session-type ev) => "session")
  (check (pi-event-session-version ev) => 3)
  (check (pi-event-session-cwd ev) => "/tmp/cwd")
  (check-true (string? (pi-event-session-id ev)))
  (check-true (string? (pi-event-session-timestamp ev)))
  ;; 兼容 alist 访问
  (check (cdr (assoc "type" ev)) => "session")
  (check (cdr (assoc "version" ev)) => 3)
  (check (cdr (assoc "cwd" ev)) => "/tmp/cwd")
  ;; 验证缺省生成 ID 符合 RFC 9562 UUIDv7 标准
  (let ((id (pi-event-session-id ev)))
    (check (string-length id) => 36)
    (check (char=? (string-ref id 8) #\-) => #t)
    (check (char=? (string-ref id 13) #\-) => #t)
    (check (char=? (string-ref id 18) #\-) => #t)
    (check (char=? (string-ref id 23) #\-) => #t)
    ;; 版本位恒为 7
    (check (char=? (string-ref id 14) #\7) => #t)
    ;; 变体位为 10xx 变体
    (check-true (and (memv (string-ref id 19) '(#\8 #\9 #\a #\b #\A #\B)) #t))
    ;; 时间戳合理性
    (let* ((hex-ts (string-append (substring id 0 8) (substring id 9 13)))
           (ts-ms (string->number hex-ts 16))
           (uuid-sec (/ ts-ms 1000.0))
           (now-sec (current-second))
           (diff (abs (- uuid-sec now-sec))))
      (check-true (< diff 120.0)))))

;; 指定 id 与 timestamp 构造
(let ((ev (pi-event-session "/tmp/cwd" "custom-id" "2026-10-18T00:00:00.000Z")))
  (check-true (pi-event-session? ev))
  (check (pi-event-session-id ev) => "custom-id")
  (check (pi-event-session-timestamp ev) => "2026-10-18T00:00:00.000Z"))

(check-report)
