;; pi-bash-preview-lines
;; pi-bash-preview-lines 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (pi-bash-preview-lines ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive bash))

(check-set-mode! 'report)

;; bash 预览行数常量对齐 pi（PREVIEW_LINES = 20）
(check pi-bash-preview-lines => 20)

(check-report)
