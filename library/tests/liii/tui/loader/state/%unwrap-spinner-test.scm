;; %unwrap-spinner
;; 解包微调器对象，获取其内部私有 <spinner-data> 结构。
;;
;; 语法
;; ----
;; (%unwrap-spinner obj caller)
;;
;; 参数
;; ----
;; obj : any
;; 待解包的对象，可为 spinner component 或 %spinner-data 记录。
;;
;; caller : string?
;; 调用方标识字符串，用于错误提示。
;;
;; 返回值
;; ----
;; %spinner-data?
;; 微调器内部数据记录。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let* ((s (make-spinner "test")) (sp (%unwrap-spinner s "test")))
  (check-true (%spinner-data? sp))
  ;; 直接传入数据记录也能解包
  (check (eq? (%unwrap-spinner sp "test") sp) => #t)
) ;let*

(check-catch 'type-error (%unwrap-spinner "not-a-spinner" "test"))

(check-report)
