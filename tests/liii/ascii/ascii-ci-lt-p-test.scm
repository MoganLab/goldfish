(import (liii check)
        (liii ascii))

;; ascii-ci<?
;; 按 ASCII 大小写无关规则比较两个字符是否为小于关系。
;;
;; 语法
;; ----
;; (ascii-ci<? char1 char2)
;;
;; 参数
;; ----
;; char1, char2 : char? | integer?
;; 要比较的字符或码点。
;;
;; 返回值
;; ----
;; boolean
;; 若大小写折叠后char1小于char2则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 只针对 ASCII 大小写折叠规则。
;;
;; 示例
;; ----
;; (ascii-ci<? #\a #\B) => #t
;;
;; 错误处理
;; ----
;; 参数类型不匹配时按过程约定报错

(check-true (ascii-ci<? #\a #\B))

(check-report)
