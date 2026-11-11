;; all-in-one-cfg-continuous?
;; 获取是否开启全链连续等候。
;;
;; 语法
;; ----
;; (all-in-one-cfg-continuous? cfg)
;;
;; 参数
;; ----
;; cfg : <all-in-one-cfg>
;; 配置记录。
;;
;; 返回值
;; ----
;; boolean
;; 是否开启连续等候。

(import (scheme base) (liii check) (liii pi all_in_one types))

(check-set-mode! 'report-failed)

(let ((c1 (make-all-in-one-cfg #t 1 1 1 '()))
      (c2 (make-all-in-one-cfg #f 1 1 1 '()))
     ) ;
  (check (all-in-one-cfg-continuous? c1) => #t)
  (check (all-in-one-cfg-continuous? c2) => #f)
) ;let

(check-report)
