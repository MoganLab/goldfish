;; pi-model-supports-reasoning?
;; 判断模型元数据条目是否支持推理（reasoning）。
;;
;; 语法
;; ----
;; (pi-model-supports-reasoning? entry)
;;
;; 参数
;; ----
;; entry : list?
;; 模型元数据关联列表。
;;
;; 返回值
;; ----
;; boolean
;; 当 reasoning 字段显式为假时返回 #f，缺失 entry 时按默认支持返回 #t。

(import (liii check) (liii pi ai thinking))

(check-set-mode! 'report-failed)

(check-true (pi-model-supports-reasoning? '(("reasoning" . #t))))
(check-true (pi-model-supports-reasoning? '((reasoning . true))))
(check-false (pi-model-supports-reasoning? '((reasoning . false))))
(check-false (pi-model-supports-reasoning? '((reasoning . #f))))
(check-false (pi-model-supports-reasoning? '((reasoning . null))))
(check-false (pi-model-supports-reasoning? '(("id" . "m"))))
(check-true (pi-model-supports-reasoning? '()))

(check-report)
