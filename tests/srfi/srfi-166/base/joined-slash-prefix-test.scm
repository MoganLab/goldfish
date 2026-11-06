;; joined/prefix
;; 前缀分隔连接组合子：每个元素前都输出分隔符。
;;
;; 语法
;; ----
;; (joined/prefix elt-f ls [sep])
;;
;; 参数
;; ----
;; elt-f : format?
;; 每个元素使用的格式化对象。
;;
;; ls : list?
;; 元素列表。
;;
;; sep : format?
;; 可选，分隔符，默认为空。
;;
;; 返回值
;; ----
;; format?
;; 依次输出「分隔符 + 元素」的格式化组合子。
;;
;; 说明
;; ----
;; 与 joined 的区别是首元素之前也输出 sep，适合生成
;; `:a:b:c` 这类每段都带前缀的串。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (joined/prefix each '(1 2 3) "-")) => "-1-2-3")
(check (show #f (joined/prefix each '(1 2 3))) => "123")
(check (show #f (joined/prefix each '())) => "")

(check-report)
