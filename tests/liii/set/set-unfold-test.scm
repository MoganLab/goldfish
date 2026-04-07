(import (liii check)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-unfold
;; 使用 unfold 模式创建 set。
;;
;; 语法
;; ----
;; (set-unfold stop? mapper successor seed comparator)
;;
;; 参数
;; ----
;; stop? : procedure
;; 停止谓词。接收当前种子，返回布尔值。
;;
;; mapper : procedure
;; 映射函数。接收当前种子，返回要添加到 set 的元素。
;;
;; successor : procedure
;; 后继函数。接收当前种子，返回下一个种子。
;;
;; seed : any
;; 初始种子值。
;;
;; comparator : comparator
;; 元素比较器。
;;
;; 返回值
;; ----
;; set
;; 返回生成的 set。
;;
;; 示例
;; ----
;; (set-unfold (lambda (x) (= x 10)) (lambda (x) x) (lambda (x) (+ x 1)) 0 comp)
;; => 包含 0 到 9 的 set

(define s-empty (set))
(define comp (set-element-comparator s-empty))

;; Create set {0, 1, 2, ..., 9}
(define s-10 (set-unfold)
               (lambda (x) (= x 10))
               (lambda (x) x)
               (lambda (x) (+ x 1))
               0
               comp
) ;define

(check-true (set-contains? s-10 0))
(check-true (set-contains? s-10 9))
(check-false (set-contains? s-10 10))

(check-report)
