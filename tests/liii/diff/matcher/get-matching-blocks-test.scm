;; get-matching-blocks
;; 计算两个行向量之间的最长匹配块序列。
;;
;; 语法
;; ----
;; (get-matching-blocks a b)
;;
;; 参数
;; ----
;; a : vector
;; 修改前的行向量。
;; b : vector
;; 修改后的行向量。
;;
;; 返回值
;; ----
;; list
;; 匹配块列表，每个块形如 (i j k)，
;; 表示 a 的第 i 行起与 b 的第 j 行起有 k 行相同；
;; 末尾附带 (la lb 0) 表示两序列的结束位置。
;;
;; 说明
;; ----
;; 匹配块按 a 中的位置升序排列，
;; 相邻且连续的匹配块会被合并为更大的块。

(import (liii check)
        (liii diff matcher))

(check-set-mode! 'report-failed)

;; 完全相同：单个覆盖全部的匹配块
(check (get-matching-blocks #("a" "b") #("a" "b")) => '((0 0 2) (2 2 0)))

;; 完全不同：只有结束位置
(check (get-matching-blocks #("a") #("b")) => '((1 1 0)))

;; 空序列
(check (get-matching-blocks #() #()) => '((0 0 0)))

;; 一侧为空
(check (get-matching-blocks #("a") #()) => '((1 0 0)))

;; 中间不同：拆分为两个匹配块
(check (get-matching-blocks #("a" "b" "c") #("a" "x" "c"))
  => '((0 0 1) (2 2 1) (3 3 0)))

(check-report)
