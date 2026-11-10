;; %syntax-case-length
;; 计算列表长度，用于宏模式中省略号匹配的边界切分。
;;
;; 语法
;; ----
;; (%syntax-case-length ls)
;;
;; 参数
;; ----
;; ls : list
;; 待求长度的列表。
;;
;; 返回值
;; -----
;; integer
;; 列表元素个数。

(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; 1. 空列表长度
(check (%syntax-case-length '()) => 0)

;; 2. 正常列表长度
(check (%syntax-case-length '(1 2 3)) => 3)

(check-report)
