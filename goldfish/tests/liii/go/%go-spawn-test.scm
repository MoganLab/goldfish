;; %go-spawn
;; 将任务直接投递到后台 worker 线程池执行，是 (liii go) 的底层投递入口。
;;
;; 语法
;; ----
;; (%go-spawn names vals code)
;;
;; 参数
;; ----
;; names : list
;; 变量名符号列表，个数必须与 vals 一致。
;; vals : list
;; 与 names 一一对应的值列表，元素必须为可序列化的数据类型。
;; code : list
;; 以 begin 包裹的表达式列表，在 worker 会话中求值；
;; worker 会话已导入 (liii go)，可直接使用 chan-send! 等接口。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. names 与 vals 在 worker 会话中以 let 绑定，code 在该绑定下求值。
;; 2. 本接口不做库前导（import 恢复），需要额外库时由 code 自行 import。
;;
;; 错误处理
;; ----
;; names 与 vals 个数不一致时抛出 value-error；
;; 变量名不是符号、值不可序列化或 code 不可序列化时抛出 type-error。

(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; 1. 基本投递：worker 会话执行 code 并把结果送回通道

(define ch1 (make-chan 1))
(%go-spawn '(ch) (list ch1) '((begin (chan-send! ch 42))))
(check (chan-recv! ch1 5000) => 42)

;; 2. 变量绑定：names 与 vals 在 worker 会话中一一对应

(define ch2 (make-chan 1))
(%go-spawn '(ch x) (list ch2 7) '((begin (chan-send! ch (* x 6)))))
(check (chan-recv! ch2 5000) => 42)

;; 3. 无变量投递：任务同样被调度执行

(%go-spawn '() '() '((begin 1)))

;; 4. 参数错误路径

;; names 与 vals 个数不一致
(check-catch 'value-error (%go-spawn '(a) '() '((begin (chan-send! a 1)))))
;; 变量名不是符号
(check-catch 'type-error (%go-spawn '(1) '(1) '((begin (chan-send! 1 1)))))
;; 值不可序列化（过程）
(check-catch 'type-error
  (%go-spawn '(x) (list (lambda (v) v)) '((begin (chan-send! x 1))))
) ;check-catch
;; code 不可序列化（过程）
(check-catch 'type-error (%go-spawn '() '() (lambda () 1)))

(check-report)
