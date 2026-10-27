(import (scheme base)
  (liii check)
  (liii pi code modes interactive thinking))

(check-set-mode! 'report-failed)

;; 按 7 级全集循环，非法输入容错回 "off"
(check (pi-next-thinking-level "off") => "minimal")
(check (pi-next-thinking-level "minimal") => "low")
(check (pi-next-thinking-level "high") => "xhigh")
(check (pi-next-thinking-level "max") => "off")
(check (pi-next-thinking-level "bogus") => "off")

(check-report)
