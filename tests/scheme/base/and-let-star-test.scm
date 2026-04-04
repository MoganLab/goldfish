(import (liii check))
(import (liii base))
(import (scheme base))

(check-set-mode! 'report-failed)

;; and-let*
;; 顺序绑定变量，如果任一绑定结果为假则立即返回假，否则执行主体。
;;
;; 语法
;; ----
;; (and-let* ((var init) ...) body)
;;
;; 参数
;; ----
;; var : symbol
;; 变量名，可以省略只保留 init 表达式。
;;
;; init : any
;; 初始值表达式。
;;
;; body : any
;; 当所有绑定都成功时执行的表达式。
;;
;; 返回值
;; -----
;; any
;; 如果任一 init 为假，立即返回 #f
;; 如果所有 init 都为真，返回 body 的结果
;;
;; 说明
;; ----
;; and-let* 结合了 and 和 let* 的功能，适合用于条件性的变量绑定。
;; 它在需要多个条件同时满足时才执行某些操作的场景中非常有用。

;; 基础测试 - 所有条件为真
(check (and-let* ((hi 3) (ho #t)) (+ hi 1)) => 4)

;; 基础测试 - 有条件为假，返回假
(check (and-let* ((hi 3) (ho #f)) (+ hi 1)) => #f)

;; 单变量测试
(check (and-let* ((x 5)) (* x 2)) => 10)

;; 空绑定 - 直接执行 body
(check (and-let* () 42) => 42)

;; 嵌套表达式
(check
  (and-let* ((a 1)
             (b (+ a 2))
             (c (* b 2)))
    (+ a b c))
  => 10)

;; 短路测试 - 第二个条件为假，不会执行第三个
(check
  (let ((evaluated #f))
    (and-let* ((x 1)
               (y #f)
               (z (begin (set! evaluated #t) 3)))
      'body)
    evaluated)
  => #f)

(check-report)
