;; pi-args-set
;; 向参数字典写入或覆盖指定键的取值。
;;
;; 语法
;; ----
;; (pi-args-set parsed key value)
;;
;; 参数
;; ----
;; parsed : alist
;; 解析后的参数字典。
;;
;; key : symbol
;; 参数键。
;;
;; value : any
;; 写入的取值。
;;
;; 返回值
;; ----
;; alist
;; 更新后的参数字典。

(import (liii check) (liii pi code cli args query))

(check-set-mode! 'report-failed)

(let* ((args '((key1 . "val1")))
       (updated (pi-args-set args 'key2 "val2"))
       (overridden (pi-args-set updated 'key1 "new1"))
      ) ;
  (check (pi-args-ref updated 'key1) => "val1")
  (check (pi-args-ref updated 'key2) => "val2")
  (check (pi-args-ref overridden 'key1) => "new1")
  (check (pi-args-ref overridden 'key2) => "val2")
) ;let*

(check-report)
