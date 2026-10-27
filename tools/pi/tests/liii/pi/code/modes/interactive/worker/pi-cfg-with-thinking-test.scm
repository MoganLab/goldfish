(import (scheme base)
  (liii check)
  (liii pi code modes interactive worker))

(check-set-mode! 'report-failed)

;; 覆盖已有项 / cfg 无该项时前置追加
(check (pi-cfg-with-thinking '((thinking-level . "off") (model-id . "m")) "high")
  => '((thinking-level . "high") (model-id . "m")))
(check (pi-cfg-with-thinking '((model-id . "m")) "low")
  => '((thinking-level . "low") (model-id . "m")))

(check-report)
