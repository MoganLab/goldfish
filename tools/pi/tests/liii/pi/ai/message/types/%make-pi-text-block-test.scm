;; %make-pi-text-block
;; 构造文本内容块 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-text-block text)
;;
;; 参数
;; ----
;; text : string?
;; 文本内容
;;
;; 返回值
;; ----
;; pi-text-block?
;; 文本内容块 record 实例。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-text-block "Hello")))
  (check-true (pi-text-block? obj))
  (check (pi-text-block-text obj) => "Hello")
) ;let

(check-report)
