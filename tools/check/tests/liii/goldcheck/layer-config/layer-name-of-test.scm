;; layer-name-of
;; 获取分层定义对象中的层名称。
;;
;; 语法
;; ----
;; (layer-name-of layer)
;;
;; 参数
;; ----
;; layer : layer   已解析的分层定义对象
;;
;; 返回值
;; ------
;; string
;; 分层的名称。

(import (scheme base) (liii check) (liii goldcheck layer-config))

(check-set-mode! 'report-failed)

;; 1. 获取分层对象的 name 属性
(let ((layer '((name . "kernel") (libs . #("liii base")))))
  (check (layer-name-of layer) => "kernel")
) ;let

(check-report)
