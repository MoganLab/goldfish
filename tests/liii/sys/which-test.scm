(import (liii check) (liii sys) (liii os))


(check-set-mode! 'report-failed)


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
;; path : string?
;;
;; 说明
;; ----
;; 找到时返回可执行文件路径字符串，找不到返回 #f。


;; ; 基本功能测试
(check (which "no-such-cmd-xyz-0143") => #f)
(when (not (os-windows?))
  (check (string? (which "sh")) => #t)
) ;when


;; ; 参数类型测试
;; cmd 必须是 string?，传入其他类型应报 wrong-type-arg，
;; 而不是把对象当作 C 字符串指针导致崩溃 (devel/0143.md)
(check-catch 'wrong-type-arg (which 123))
(check-catch 'wrong-type-arg (which 'ls))
(check-catch 'wrong-type-arg (which "ls" 123))
;; C 层入口 g_which 走同一实现，两个参数都需要类型检查
(check-catch 'wrong-type-arg (g_which 123))
(check-catch 'wrong-type-arg (g_which "ls" 123))


(check-report)
