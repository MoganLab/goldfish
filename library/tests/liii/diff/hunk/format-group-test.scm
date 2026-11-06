;; format-group
;; 把一组差异操作码格式化为统一差异（Unified Diff）的 hunk 行列表。
;;
;; 语法
;; ----
;; (format-group group a b)
;;
;; 参数
;; ----
;; group : list
;; 差异操作码列表，每个操作码形如 (tag i1 i2 j1 j2)，
;; tag 取值 equal、replace、delete、insert。
;; a : vector
;; 修改前的行向量。
;; b : vector
;; 修改后的行向量。
;;
;; 返回值
;; ----
;; list
;; 字符串列表，首行为 hunk 头 "@@ -范围 +范围 @@"，
;; 其后为带前缀（空格、- 或 +）的差异行。
;;
;; 说明
;; ----
;; equal 操作码输出以空格为前缀的上下文行，
;; replace 输出 "-" 与 "+" 两组行，
;; delete 只输出 "-" 行，insert 只输出 "+" 行。

(import (liii check)
        (liii diff hunk))

(check-set-mode! 'report-failed)

;; equal：上下文行以空格前缀
(check (format-group '((equal 0 1 0 1)) #("x") #("x"))
  => '("@@ -1 +1 @@" " x"))

;; replace：先输出 "-" 行再输出 "+" 行
(check (format-group '((replace 0 1 0 1)) #("x") #("y"))
  => '("@@ -1 +1 @@" "-x" "+y"))

;; delete：只输出 "-" 行
(check (format-group '((delete 0 1 0 0)) #("x") #())
  => '("@@ -1 +0,0 @@" "-x"))

;; insert：只输出 "+" 行
(check (format-group '((insert 0 0 0 1)) #() #("y"))
  => '("@@ -0,0 +1 @@" "+y"))

;; 多行区间与多操作码组合
(check (format-group '((equal 0 2 0 2) (replace 2 3 2 3)) #("a" "b" "c") #("a" "b" "d"))
  => '("@@ -1,3 +1,3 @@" " a" " b" "-c" "+d"))

(check-report)
