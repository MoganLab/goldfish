;; get-opcodes
;; 把匹配块序列转换为差异操作码序列。
;;
;; 语法
;; ----
;; (get-opcodes matching-blocks)
;;
;; 参数
;; ----
;; matching-blocks : list
;; 匹配块列表，每个块形如 (i j k)。
;;
;; 返回值
;; ----
;; list
;; 操作码列表，每个操作码形如 (tag i1 i2 j1 j2)，
;; tag 取值 equal、replace、delete、insert，
;; 区间使用 0 基的半开区间下标。

(import (liii check)
        (liii diff matcher))

(check-set-mode! 'report-failed)

;; 完全相同：单个 equal 操作码
(check (get-opcodes '((0 0 2) (2 2 0))) => '((equal 0 2 0 2)))

;; 完全不同：单个 replace 操作码
(check (get-opcodes '((1 1 0))) => '((replace 0 1 0 1)))

;; 中间不同：equal + replace + equal
(check (get-opcodes '((0 0 1) (2 2 1) (3 3 0)))
  => '((equal 0 1 0 1) (replace 1 2 1 2) (equal 2 3 2 3)))

;; 空匹配块
(check (get-opcodes '()) => '())

(check-report)
