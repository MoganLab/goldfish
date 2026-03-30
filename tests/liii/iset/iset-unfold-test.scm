(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-unfold
;; 使用 unfold 模式创建整数集合。
;;
;; 语法
;; ----
;; (iset-unfold stop? mapper successor seed)
;;
;; 参数
;; ----
;; stop? : procedure
;; 停止谓词。接收当前种子，返回布尔值。
;;
;; mapper : procedure
;; 映射函数。接收当前种子，返回要添加到集合的整数。
;;
;; successor : procedure
;; 后继函数。接收当前种子，返回下一个种子。
;;
;; seed : any
;; 初始种子值。
;;
;; 返回值
;; -----
;; 返回生成的 iset。
;;
(check (iset->list (iset-unfold (lambda (n) (> n 64))
                                values
                                (lambda (n) (* n 2))
                                2))
       => '(2 4 8 16 32 64)
) ;check

(check-report)
