(import (scheme base)
  (liii check)
  (liii pi code modes interactive bash))

(check-set-mode! 'report)

;; bash 预览行数常量对齐 pi（PREVIEW_LINES = 20）
(check pi-bash-preview-lines => 20)

(check-report)
