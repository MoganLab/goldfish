(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-format-age：把 ISO-8601 时间戳格式化为相对时长
;; 可选 now-posix 参数固定"当前时间"便于测试；无法解析时返回空串
;; 档位：now / Nm / Nh / Nd / Nw / Nmo / Ny

(define NOW 1793077200) ; 2026-10-27T05:00:00Z

(check (session-selector-format-age "2026-10-27T04:59:30.000Z" NOW) => "now")
(check (session-selector-format-age "2026-10-27T04:15:00.000Z" NOW) => "45m")
(check (session-selector-format-age "2026-10-26T23:00:00.000Z" NOW) => "6h")
(check (session-selector-format-age "2026-10-20T08:00:02.000Z" NOW) => "6d")
(check (session-selector-format-age "2026-10-07T05:00:00.000Z" NOW) => "2w")
(check (session-selector-format-age "2026-07-19T05:00:00.000Z" NOW) => "3mo")
(check (session-selector-format-age "2025-06-14T05:00:00.000Z" NOW) => "1y")

;; 未来时间戳钳制为 now
(check (session-selector-format-age "2026-10-28T05:00:00.000Z" NOW) => "now")

;; 无法解析的输入返回空串而不是报错
(check (session-selector-format-age "not-a-date" NOW) => "")

(check-report)
