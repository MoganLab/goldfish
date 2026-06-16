(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-from-string
;; 从字符串构造路径值，与 path 函数等价。
;;
;; 语法
;; ----
;; (path-from-string string-path)
;;
;; 参数
;; ----
;; string-path : string?
;; 字符串形式的路径。
;;
;; 返回值
;; ----
;; path-value
;; 返回对应的路径值。
;;
;; 示例
;; ----
;; (path->string (path-from-string "archive.tar.gz")) => "archive.tar.gz"

(check (path->string (path-from-string "archive.tar.gz")) => "archive.tar.gz")

(when (os-windows?)
  ;; s7 的 port-filename / argv 在 Windows 上返回正斜杠绝对路径,
  ;; parse-windows-path 必须同时识别 / 和 \,否则 string-split-vec 一刀不切
  (check (path->string (path-from-string "C:/Users/foo/bar.scm"))
    =>
    "C:\\Users\\foo\\bar.scm"
  ) ;check
) ;when

(check-report)
