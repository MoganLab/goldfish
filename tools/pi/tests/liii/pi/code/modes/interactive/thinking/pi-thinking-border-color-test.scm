(import (scheme base)
  (liii check)
  (liii pi code modes interactive thinking))

(check-set-mode! 'report-failed)

;; thinking 边框颜色映射（对齐上游 pi dark 主题实测 RGB）
(check (pi-thinking-border-color "off") => "\x1b;[38;2;102;111;116m")
(check (pi-thinking-border-color "minimal") => "\x1b;[38;2;97;120;133m")
(check (pi-thinking-border-color "low") => "\x1b;[38;2;78;130;156m")
(check (pi-thinking-border-color "medium") => "\x1b;[38;2;91;127;200m")
(check (pi-thinking-border-color "high") => "\x1b;[38;2;151;118;229m")
(check (pi-thinking-border-color "xhigh") => "\x1b;[38;2;219;72;187m")
(check (pi-thinking-border-color "max") => "\x1b;[38;2;254;65;87m")
(check (pi-thinking-border-color "bogus") => "\x1b;[38;2;102;111;116m")

(check-report)
