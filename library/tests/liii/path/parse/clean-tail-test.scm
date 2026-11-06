;; clean-tail
;; 过滤 anchor 之后路径段中的空段与分隔符残段。
;;
;; 语法
;; ----
;; (clean-tail parts)
;;
;; 参数
;; ----
;; parts : vector?
;; 首段为 anchor 的路径段向量。
;;
;; 返回值
;; ----
;; vector?
;; 去掉首段后，过滤空串、"/" 与 "\\" 残段的新向量。

(import (liii check) (liii path parse))

(check-set-mode! 'report-failed)

(check (clean-tail #("/" "a" "" "/" "b")) => #("a" "b"))
(check (clean-tail #("\\" "a" "\\")) => #("a"))
(check (clean-tail #("/")) => #())

(check-report)
