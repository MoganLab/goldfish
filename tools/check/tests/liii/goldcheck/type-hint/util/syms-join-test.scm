;; syms-join
;; 将符号列表中的各符号转为字符串并以空格拼接。
;;
;; 语法
;; ----
;; (syms-join syms)
;;
;; 参数
;; ----
;; syms : list of symbol  符号列表
;;
;; 返回值
;; ------
;; string
;; 拼接后的字符串。

(import (scheme base) (liii check) (liii goldcheck type-hint util))

(check-set-mode! 'report-failed)

;; 1. 多个符号拼接
(check (syms-join '(liii goldcheck type-hint)) => "liii goldcheck type-hint")

;; 2. 单个符号
(check (syms-join '(base)) => "base")

;; 3. 空符号列表
(check (syms-join '()) => "")

(check-report)
