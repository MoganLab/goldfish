;; %syntax-case-split-at
;; 在指定索引位置将列表分割为前缀与后缀两部分。
;;
;; 语法
;; ----
;; (%syntax-case-split-at ls n)
;;
;; 参数
;; ----
;; ls : list
;; 待分割的列表。
;;
;; n : integer
;; 分割点位置索引。
;;
;; 返回值
;; -----
;; pair
;; (prefix . suffix) 点对。

(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; 1. 分割列表
(check (%syntax-case-split-at '(1 2 3 4 5) 2) => '((1 2) 3 4 5))

;; 2. 在 0 处分割
(check (%syntax-case-split-at '(1 2 3) 0) => '(() 1 2 3))

(check-report)
