;; gfproject-find-tool-root
;; 查找工具的根目录路径。
;;
;; 语法
;; ----
;; (gfproject-find-tool-root tool-name [custom-dir] [opt-gf-lib]) -> string or #f
;;
;; 参数
;; ----
;; tool-name : string
;; 工具名称。
;;
;; custom-dir : string or #f (可选)
;; 自定义工具目录。
;;
;; opt-gf-lib : string (可选)
;; goldfish 库根路径。
;;
;; 返回值
;; ----
;; string or #f
;; 找到时返回工具根目录的绝对或相对路径，否则返回 #f。

(import (liii check)
  (liii path)
  (liii gfproject tools)
) ;import

(check-set-mode! 'report-failed)

(let ((root (gfproject-find-tool-root "doc")))
  (check-true (string? root))
  (check-true (path-dir? root))
) ;let

(check (gfproject-find-tool-root "non-existent-tool-xyz") => #f)
(check (gfproject-find-tool-root "version" "~/non_existent_tools_dir_xyz") => #f)
(check (gfproject-find-tool-root "doc" #f) => (gfproject-find-tool-root "doc"))

(check-report)
