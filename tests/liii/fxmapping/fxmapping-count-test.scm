(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-count
;; 统计满足谓词的键值对数量。
;;
;; 语法
;; ----
;; (fxmapping-count pred fxmap)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数，接收 key 和 value，返回布尔值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回满足 pred 的键值对数量。
;;
(check (fxmapping-count (lambda (k v) (> k 5))
         (fxmapping 3 'a 7 'b 10 'c)
       ) ;fxmapping-count
  =>
  2
) ;check
(check (fxmapping-count (lambda (k v) (symbol? v))
         (fxmapping 0 'a 1 2 2 'c)
       ) ;fxmapping-count
  =>
  2
) ;check

(check-report)
