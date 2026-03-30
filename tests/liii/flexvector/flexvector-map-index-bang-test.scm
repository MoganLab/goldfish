(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-map/index!
;; 带索引破坏性映射操作。
;;
;; 语法
;; ----
;; (flexvector-map/index! proc fv)
;; (flexvector-map/index! proc fv1 fv2 ...)
;;
;; 注意：此函数在测试文件中暂无测试代码

(check-report)
