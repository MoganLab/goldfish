;; format-all-groups
;; 把多组差异操作码统一格式化为完整 hunk 行列表。
;;
;; 语法
;; ----
;; (format-all-groups groups vec-a vec-b)
;;
;; 参数
;; ----
;; groups : list
;; hunk 分组列表，每组是若干差异操作码组成的列表。
;; vec-a : vector
;; 修改前的行向量。
;; vec-b : vector
;; 修改后的行向量。
;;
;; 返回值
;; ----
;; list
;; 字符串列表，每个分组依次输出其 hunk 头与差异行。

(import (liii check)
        (liii diff hunk))

(check-set-mode! 'report-failed)

;; 空分组列表返回空列表
(check (format-all-groups '() #("x") #("x")) => '())

;; 单个分组
(check (format-all-groups '(((equal 0 1 0 1))) #("x") #("x"))
  => '("@@ -1 +1 @@" " x"))
(check (format-all-groups '(((replace 0 1 0 1))) #("x") #("y"))
  => '("@@ -1 +1 @@" "-x" "+y"))

;; 多个分组按顺序拼接
(check (format-all-groups '(((equal 0 1 0 1)) ((replace 1 2 1 2))) #("a" "b") #("a" "c"))
  => '("@@ -1 +1 @@" " a" "@@ -2 +2 @@" "-b" "+c"))

(check-report)
