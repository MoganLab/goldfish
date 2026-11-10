;; fn
;; 从普通过程构造格式化组合子，并按位置注入格式化环境。
;;
;; 语法
;; ----
;; (fn (port row col string-width) body ...)
;;
;; 参数
;; ----
;; port : output-port?
;; 当前输出端口。
;;
;; row : exact-integer?
;; 当前行号。
;;
;; col : exact-integer?
;; 当前列号。
;;
;; string-width : (string -> exact-integer?)
;; 计算字符串显示宽度的过程。
;;
;; 返回值
;; ----
;; format?
;; 调用时才向格式化环境索取上述值的组合子。
;;
;; 说明
;; ----
;; 形参表可以只写前缀部分，例如 `(fn (col) ...)`；
;; 需要读取 row、col、width 等状态变量时必须走 fn 形参，
;; 直接把状态变量当普通变量用只会取到变量对象本身。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; col 随已输出内容推进
(check (show #f "abc" (fn (col) (each "[" col "]"))) => "abc[3]")

;; 行号在换行后递增
(check (show #f "a" nl (fn (row) (each "[" row "]"))) => "a\n[1]")

;; 只取所需形参也可以
(check (show #f (fn (port row col) (each "c=" col))) => "c=0")

;; 自定义 string-width 用于宽度敏感组合子
(check
  (show #f (fn (port row col string-width) (each (string-width "日本"))))
  =>
  "6"
) ;check

(check-report)
