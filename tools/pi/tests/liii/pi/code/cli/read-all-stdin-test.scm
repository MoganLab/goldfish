;; read-all-stdin
;; 从标准输入读取全部内容并修剪两端空白字符。
;;
;; 语法
;; ----
;; (read-all-stdin)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string? | boolean?
;; 若输入修剪后非空则返回字符串，若为空或仅包含空白字符则返回 #f。

;; 先导入 (liii pi code cli) 避免宏展开期常量回收
(import (liii pi code cli) (liii check))

(check-set-mode! 'report-failed)

;; 正常非空字符串输入
(check (with-input-from-string "hello world\n" read-all-stdin) => "hello world")
(check (with-input-from-string "  leading and trailing  \n" read-all-stdin)
  =>
  "leading and trailing"
) ;check

;; 纯空白或空输入返回 #f
(check (with-input-from-string "" read-all-stdin) => #f)
(check (with-input-from-string "   \t\r\n" read-all-stdin) => #f)


(check-report)
