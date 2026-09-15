(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gdelete
;; 从生成器产出的序列中删除与指定值相等的元素。
;;
;; 语法
;; ----
;; (gdelete item gen)
;; (gdelete item gen ==)
;;
;; 参数
;; ----
;; item : any
;; 待删除的目标元素。
;;
;; gen : procedure
;; 输入生成器。
;;
;; == : procedure (可选)
;; 判断两元素是否相等的比较谓词，默认为 equal?。
;;
;; 返回值
;; ----
;; procedure
;; 新生成器过程。跳过所有与 item 相等的元素后产出剩余元素。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gdelete 2 (generator 1 2 3 2 4))))
  (check (generator->list g) => '(1 3 4))
)

(let ((g (gdelete "a" (generator "a" "b" "a" "c") string=?)))
  (check (generator->list g) => '("b" "c"))
)

(check-report)
