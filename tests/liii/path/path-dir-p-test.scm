(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-dir?
;; 判断给定路径是否为目录。
;;
;; 语法
;; ----
;; (path-dir? path)
;;
;; 参数
;; ----
;; path : string? | path-value
;; 文件或目录路径。
;;
;; 返回值
;; ----
;; boolean
;; 当路径存在且为目录时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; "" 空字符串返回 #f
;; "." 当前目录返回 #t
;; ".." 上级目录返回 #t

;; 基本功能测试
(check (path-dir? ".") => #t)
(check (path-dir? (path ".")) => #t)
(check (path-dir? "..") => #t)

;; 边界情况测试
(check (path-dir? "") => #f)
(check (path-dir? "nonexistent") => #f)
(check (path-dir? "#\\null") => #f)

;; 临时目录测试
(check-true (path-dir? (path-temp-dir)))
(check-true (path-dir? (path->string (path-temp-dir))))

(when (not (os-windows?))
  ;; 根目录与常用目录测试
  (check (path-dir? "/") => #t)
  (check (path-dir? "/tmp") => #t)
  (check (path-dir? "/etc") => #t)
  ;; 不存在目录测试
  (check (path-dir? "/no_such_dir") => #f)
  (check (path-dir? "/not/a/real/path") => #f)
) ;when

(when (os-windows?)
  ;; 根目录与常用目录测试
  (check (path-dir? "C:/") => #t)
  (check (path-dir? "C:/Windows") => #t)
  (check (path-dir? "C:/Program Files") => #t)
  ;; 不存在目录测试
  (check (path-dir? "C:/no_such_dir/") => #f)
  (check (path-dir? "Z:/definitely/not/exist") => #f)
) ;when

(check-report)
