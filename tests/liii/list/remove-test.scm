(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; remove 函数测试
;;
;; 语法
;; ----
;; (remove pred list)
;;
;; 参数
;; ----
;; pred : procedure?
;; 谓词函数，接受单个参数并返回布尔值。
;;
;; list : list?
;; 要处理的列表。
;;
;; 返回值
;; ------
;; list
;; 返回不满足谓词条件的元素组成的新列表。
;;
;; 说明
;; ----
;; remove函数与filter函数功能相反，返回不满足谓词条件的元素。
;;
;; 示例
;; ----
;; (remove even? '(-2 -1 0 1 2)) => '(-1 1)


(check (remove even? '(-2 -1 0 1 2))
  =>
  '(-1 1)
) ;check


(check-report)
