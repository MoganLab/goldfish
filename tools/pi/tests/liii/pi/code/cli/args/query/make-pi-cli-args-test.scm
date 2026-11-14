;; make-pi-cli-args
;; 构造带默认值与关键字参数的 <pi-cli-args> 记录。
;;
;; 语法
;; ----
;; (make-pi-cli-args [keyword val ...]) -> <pi-cli-args>
;;
;; 参数
;; ----
;; keyword val : any（可选）
;; 命令行参数字段的关键字与取值，未传入的字段取默认值。
;;
;; 返回值
;; ------
;; <pi-cli-args>
;; 初始化后的命令行参数记录。

(import (liii check) (liii pi code cli args query))

(check-set-mode! 'report-failed)

(let ((args (make-pi-cli-args :provider "deepseek" :extensions '((custom-flag . "abc")))))
  (check-true (pi-cli-args? args))
  (check (pi-args-ref args 'provider) => "deepseek")
  (check (pi-args-ref args 'model) => #f)
  (check (pi-args-ref args 'skill-paths) => '())
  (check (pi-args-ref args 'custom-flag) => "abc")
) ;let

(check-report)
