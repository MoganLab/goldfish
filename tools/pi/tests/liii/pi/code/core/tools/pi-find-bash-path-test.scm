(import (liii check) (liii pi code core tools bash))

(check-set-mode! 'report-failed)

;; pi-find-bash-path
;; 探测当前平台下的 bash 可执行文件路径。
;;
;; 语法
;; ----
;; (pi-find-bash-path [os]) -> string
;;
;; 参数
;; ----
;; os : symbol
;; 可选的平台标识，支持 windows 和 posix；缺省时自动检测。
;;
;; 返回值
;; ----
;; string
;; 已存在的 bash 路径；无法找到安装路径时返回 "bash"，交由系统 PATH 解析。
;;
;; 示例
;; ----
;; (pi-find-bash-path 'posix) => "/bin/bash"
(let ((path (pi-find-bash-path)))
  (check-true (string? path))
  (check-true (> (string-length path) 0))
) ;let

;; 验证模拟 posix 平台探测
(let ((posix-path (pi-find-bash-path 'posix)))
  (check-true (string? posix-path))
  (check-true (> (string-length posix-path) 0))
) ;let

;; 验证模拟 windows 平台探测
(let ((win-path (pi-find-bash-path 'windows)))
  (check-true (string? win-path))
  (check-true (> (string-length win-path) 0))
) ;let

(check-report)
