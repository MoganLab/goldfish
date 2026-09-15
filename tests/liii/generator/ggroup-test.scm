(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; ggroup
;; 将生成器产出的元素按每 k 个打包为一个子列表。支持用填充值补齐末尾不足 k 个的组。
;;
;; 语法
;; ----
;; (ggroup gen k)
;; (ggroup gen k padding)
;;
;; 参数
;; ----
;; gen : procedure
;; 输入生成器。
;;
;; k : exact-positive-integer
;; 每个分组的元素大小。
;;
;; padding : any (可选)
;; 若提供，当最后一组不足 k 个元素时，使用 padding 补足到 k 个。
;;
;; 返回值
;; ----
;; procedure
;; 新生成器过程。每次产出一个长度为 k（或未填充时末尾可能不足 k）的列表。
;;
;; 错误处理
;; ----
;; 无

(let ((g (ggroup (generator 1 2 3 4 5) 2)))
  (check (generator->list g) => '((1 2) (3 4) (5)))
)

(let ((g (ggroup (generator 1 2 3 4 5) 3 #f)))
  (check (generator->list g) => '((1 2 3) (4 5 #f)))
)

(check-report)
