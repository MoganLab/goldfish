(import (liii check) (liii pi code core tools bash))

(check-set-mode! 'report-failed)

;; pi-find-bash-path
;; 探测当前平台下的 bash 可执行文件路径。
(let ((path (pi-find-bash-path)))
  (check-true (string? path))
  (check-true (> (string-length path) 0)))

;; 验证模拟 posix 平台探测
(let ((posix-path (pi-find-bash-path 'posix)))
  (check-true (string? posix-path))
  (check-true (> (string-length posix-path) 0)))

;; 验证模拟 windows 平台探测
(let ((win-path (pi-find-bash-path 'windows)))
  (check-true (string? win-path))
  (check-true (> (string-length win-path) 0)))

(check-report)
