;; get-grouped-opcodes
;; 把差异操作码按上下文行数聚合为 hunk 分组。
;;
;; 语法
;; ----
;; (get-grouped-opcodes codes n)
;;
;; 参数
;; ----
;; codes : list
;; 差异操作码列表，每个操作码形如 (tag i1 i2 j1 j2)。
;; n : exact-integer?
;; 上下文保留行数，必须为非负整数。
;;
;; 返回值
;; ----
;; list
;; hunk 分组列表，每组是若干操作码组成的列表；
;; 仅包含 equal 操作码的分组不会输出。

(import (liii check)
        (liii diff matcher))

(check-set-mode! 'report-failed)

;; 纯 equal 操作码不产生分组
(check (get-grouped-opcodes '((equal 0 3 0 3)) 3) => '())

;; 单个 replace 产生一个分组
(check (get-grouped-opcodes '((replace 1 2 1 2)) 3) => '(((replace 1 2 1 2))))

;; equal + replace + equal 聚合为同一分组
(check (get-grouped-opcodes '((equal 0 1 0 1) (replace 1 2 1 2) (equal 2 3 2 3)) 3)
  => '(((equal 0 1 0 1) (replace 1 2 1 2) (equal 2 3 2 3))))

;; 上下文行数为 0 时首尾 equal 区间被裁剪为空区间
(check (get-grouped-opcodes '((equal 0 1 0 1) (replace 1 2 1 2)) 0)
  => '(((equal 1 1 1 1) (replace 1 2 1 2))))

(check-report)
