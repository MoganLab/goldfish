(import (scheme base)
  (liii check)
  (liii pi ai retry)
  (liii date-time)
)

(check-set-mode! 'report-failed)

;; 1. 整数秒数字符串
(check (pi-parse-retry-after "5") => 5.0)
(check (pi-parse-retry-after "120") => 120.0)
(check (pi-parse-retry-after "0") => 0.0)

;; 2. 小数秒数字符串与空白包围
(check (pi-parse-retry-after " 2.5 ") => 2.5)
(check (pi-parse-retry-after "0.5") => 0.5)

;; 3. 直接传入数字类型
(check (pi-parse-retry-after 10) => 10.0)
(check (pi-parse-retry-after 3.5) => 3.5)
(check (pi-parse-retry-after 0) => 0.0)

;; 4. 负数与非法数字安全回退为 #f
(check (pi-parse-retry-after "-5") => #f)
(check (pi-parse-retry-after -1) => #f)
(check (pi-parse-retry-after "abc") => #f)
(check (pi-parse-retry-after "") => #f)
(check (pi-parse-retry-after #f) => #f)
(check (pi-parse-retry-after '()) => #f)

;; 5. HTTP-date 格式解析（RFC 1123 / RFC 7231）
;; 构造标准 UTC 时间：2026-10-21 07:28:00 GMT
(let* ((target-ts (make-timestamp 2026 10 21 7 28 0 (utc-timezone)))
       (target-sec (timestamp->utc-posix-time target-ts)))
  ;; 传入 mock 的当前时间为目标时间前 15 秒
  (let ((now (- target-sec 15)))
    (check (pi-parse-retry-after "Wed, 21 Oct 2026 07:28:00 GMT" now) => 15.0))
  ;; 传入 mock 的当前时间晚于目标时间（已过期），应返回 0.0
  (let ((now (+ target-sec 10)))
    (check (pi-parse-retry-after "Wed, 21 Oct 2026 07:28:00 GMT" now) => 0.0)))

;; 6. 畸形 HTTP 日期安全回退为 #f
(check (pi-parse-retry-after "Invalid, 99 Foo 9999 99:99:99 GMT") => #f)

(check-report)
