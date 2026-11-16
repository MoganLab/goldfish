;; %to-block-list
;; 将 content 转换为标准 pi-block 列表。
;;
;; 语法
;; ----
;; (%to-block-list c)
;;
;; 参数
;; ----
;; c : any?
;; vector、list 或 string。
;;
;; 返回值
;; ----
;; list?
;; 归一化后的块列表。

(import (liii check) (liii pi ai message) (liii pi agent tools-compat))

(check-set-mode! 'report-failed)

(let ((res (%to-block-list "hello")))
  (check (length res) => 1)
  (check-true (pi-text-block? (car res)))
  (check (pi-text-block-text (car res)) => "hello")
) ;let

(check-report)
