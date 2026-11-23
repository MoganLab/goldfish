;; indent-lines
;; 给非空行左侧补 n 个空格；空行保持为空。
;;
;; 语法
;; ----
;; (indent-lines n lines)
;;
;; 参数
;; ----
;; n : integer
;; 缩进空格数。
;; lines : list
;; 逻辑行字符串列表。
;;
;; 返回值
;; ----
;; list
;; 缩进后的行列表。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown util))

(check-set-mode! 'report-failed)

(check-true (procedure? indent-lines))

(check (indent-lines 2 '()) => '())
(check (indent-lines 2 '("a")) => '("  a"))
(check (indent-lines 3 '("a" "" "b")) => '("   a" "" "   b"))
(check (indent-lines 0 '("a")) => '("a"))

(check-report)
