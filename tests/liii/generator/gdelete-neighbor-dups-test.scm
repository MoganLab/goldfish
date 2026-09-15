(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gdelete-neighbor-dups
;; 删除生成器产出序列中相邻的重复元素。
;;
;; 语法
;; ----
;; (gdelete-neighbor-dups gen)
;; (gdelete-neighbor-dups gen ==)
;;
;; 参数
;; ----
;; gen : procedure
;; 输入生成器。
;;
;; == : procedure (可选)
;; 判断相邻元素是否相等的比较谓词，默认为 equal?。
;;
;; 返回值
;; ----
;; procedure
;; 新生成器过程。连续出现相同的相邻元素仅保留首个。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gdelete-neighbor-dups (generator 1 1 2 2 1) =)))
  (check (generator->list g) => '(1 2 1))
)

(let ((g (gdelete-neighbor-dups (generator "a" "a" "b" "b" "b" "a"))))
  (check (generator->list g) => '("a" "b" "a"))
)

(let ((g (gdelete-neighbor-dups (generator))))
  (check (generator->list g) => '())
)

(check-report)
