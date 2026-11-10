;; %cfg-override
;; 覆盖配置 alist 中的指定键值对，优先使用新 pairs，其余沿用原配置。
;;
;; 语法
;; ----
;; (%cfg-override cfg pairs)
;;
;; 参数
;; ----
;; cfg : list
;; 基础配置 alist。
;; pairs : list
;; 待覆盖或追加的键值对列表。
;;
;; 返回值
;; ----
;; list
;; 合并覆盖后的配置 alist。

(import (scheme base) (liii check) (liii pi code modes interactive worker))

(check-set-mode! 'report-failed)

;; 1. 覆盖既有配置项并保留未被覆盖的配置项
(let* ((cfg '((a . 1) (b . 2) (c . 3)))
       (pairs '((b . 20) (d . 40)))
       (result (%cfg-override cfg pairs))
      ) ;
  (check (cdr (assoc 'a result)) => 1)
  (check (cdr (assoc 'b result)) => 20)
  (check (cdr (assoc 'c result)) => 3)
  (check (cdr (assoc 'd result)) => 40)
) ;let*

(check-report)
