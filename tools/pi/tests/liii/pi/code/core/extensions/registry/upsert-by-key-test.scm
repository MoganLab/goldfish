(import (liii check) (liii pi code core extensions registry base))

(check-set-mode! 'report-failed)

;; upsert-by-key
;; 按字符串键更新列表中的元素；不存在时追加到末尾。
;;
;; 语法
;; ----
;; (upsert-by-key items key-fn new-item) -> list
;;
;; 参数
;; ----
;; items : list
;; 原元素列表。
;; key-fn : procedure
;; 从元素中提取字符串键的一元函数。
;; new-item : any
;; 用于替换同键元素或追加的新元素。
;;
;; 返回值
;; ----
;; list
;; 保持原顺序的新列表；所有同键元素都会替换为 new-item。
;;
;; 示例
;; ----
;; (upsert-by-key '(("a" . 1) ("b" . 2)) car '("b" . 3))
;; => (("a" . 1) ("b" . 3))

(check (upsert-by-key '() car '("a" . 1)) => '(("a" . 1)))
(check (upsert-by-key '(("a" . 1) ("b" . 2)) car '("b" . 3))
  =>
  '(("a" . 1) ("b" . 3))
) ;check
(check (upsert-by-key '(("a" . 1) ("b" . 2)) car '("c" . 3))
  =>
  '(("a" . 1) ("b" . 2) ("c" . 3))
) ;check
(check (upsert-by-key '(("a" . 1) ("b" . 2) ("a" . 3)) car '("a" . 4))
  =>
  '(("a" . 4) ("b" . 2) ("a" . 4))
) ;check

(check-report)
