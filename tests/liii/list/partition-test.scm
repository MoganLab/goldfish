(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; partition 函数测试
;;
;; 语法
;; ----
;; (partition pred list)
;;
;; 参数
;; ----
;; pred : procedure?
;; 谓词函数，接受单个参数并返回布尔值。
;;
;; list : list?
;; 要分区的列表。
;;
;; 返回值
;; ------
;; pair
;; 返回一个点对，car是满足谓词的元素列表，cdr是不满足谓词的元素列表。
;;
;; 示例
;; ----
;; (partition symbol? '(one 2 3 four five 6)) => (cons '(five four one) '(6 3 2))

(check
  (partition symbol? '(one 2 3 four five 6))
  => (cons '(five four one) '(6 3 2))
) ;check

(check-report)
