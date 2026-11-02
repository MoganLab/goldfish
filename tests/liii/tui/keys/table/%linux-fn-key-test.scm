;; %linux-fn-key
;; 将 Linux 控制台 ESC [ [ X 序列中的功能键字节转换为功能键名。
;;
;; 语法
;; ----
;; (%linux-fn-key char-byte)
;;
;; 参数
;; ----
;; char-byte : integer?，序列中 CSI 后的 ASCII 字节值
;;
;; 返回值
;; ----
;; 已知功能键字节返回 f1 到 f5 的按键符号，未知字节返回 #f。

(import (liii check) (liii tui keys table))

(check-set-mode! 'report-failed)

;; Linux 控制台功能键 F1 ~ F5
(check (%linux-fn-key 65) => 'f1)
(check (%linux-fn-key 66) => 'f2)
(check (%linux-fn-key 67) => 'f3)
(check (%linux-fn-key 68) => 'f4)
(check (%linux-fn-key 69) => 'f5)

;; 未知字节
(check (%linux-fn-key 70) => #f)
(check (%linux-fn-key 97) => #f)
(check (%linux-fn-key 0) => #f)

(check-report)
