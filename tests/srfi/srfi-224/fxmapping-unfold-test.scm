(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-unfold
;; 使用 unfold 模式创建整数映射。
;;
;; 语法
;; ----
;; (fxmapping-unfold stop? mapper successor seed)
;;
;; 参数
;; ----
;; stop? : procedure
;; 停止谓词。接收当前种子，返回布尔值。
;;
;; mapper : procedure
;; 映射函数。接收当前种子，返回两个值：key 和 value。
;;
;; successor : procedure
;; 后继函数。接收当前种子，返回下一个种子。
;;
;; seed : any
;; 初始种子值。
;;
;; 返回值
;; -----
;; 返回生成的 fxmapping。
;;
(check (fxmapping-ref (fxmapping-unfold (lambda (n) (> n 3))
                        (lambda (n) (values n (* n 10)))
                        (lambda (n) (+ n 1))
                        0
                      ) ;fxmapping-unfold
         2
         (lambda () 'not-found)
       ) ;fxmapping-ref
  =>
  20
) ;check
(check (fxmapping-ref (fxmapping-unfold (lambda (n) (> n 3))
                        (lambda (n) (values n (* n 10)))
                        (lambda (n) (+ n 1))
                        0
                      ) ;fxmapping-unfold
         5
         (lambda () #f)
       ) ;fxmapping-ref
  =>
  #f
) ;check

(check-report)
