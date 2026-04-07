(import (liii check)
        (liii ascii)
) ;import

;; ascii-string-ci<?
;; 按 ASCII 大小写无关规则比较两个字符串是否为小于关系。
;;
;; 语法
;; ----
;; (ascii-string-ci<? string1 string2)
;;
;; 参数
;; ----
;; string1, string2 : string?
;; 要比较的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 若大小写折叠后string1小于string2则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 比较与最短可判定前缀长度线性相关。
;;
;; 示例
;; ----
;; (ascii-string-ci<? "abc" "ABD") => #t
;;
;; 错误处理
;; ----
;; 参数类型不匹配时按过程约定报错

(check-true (ascii-string-ci<? "abc" "ABD"))

(check-report)
