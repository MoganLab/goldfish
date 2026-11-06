;; which
;; 在 PATH 或指定目录中查找可执行文件。
;;
;; 语法
;; ----
;; (which cmd)
;; (which cmd path)
;;
;; 参数
;; ----
;; cmd : string?
;; 要查找的可执行文件名。
;; path : string?
;; 可选的查找目录，缺省时在 PATH 环境变量指定的目录中查找。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 找到时返回可执行文件的完整路径字符串，找不到返回 #f。
;;
;; 错误处理
;; ----
;; cmd 不是字符串时抛出 type-error。

(import (liii check)
        (liii os)
        (liii shutil))


(check-set-mode! 'report-failed)


;; ; 基本功能测试
(check (which "no-such-cmd-xyz-0143") => #f)
(when (not (os-windows?))
  (check (string? (which "sh")) => #t)
) ;when


;; ; 参数类型测试
;; cmd 必须是 string?，传入其他类型应报 type-error，
;; 而不是把对象当作 C 字符串指针导致崩溃 (devel/0143.md)
(check-catch 'type-error (which 123))
(check-catch 'type-error (which 'ls))
(check-catch 'type-error (which "ls" 123))


(check-report)
