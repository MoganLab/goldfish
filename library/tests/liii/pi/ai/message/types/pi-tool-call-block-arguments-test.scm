;; pi-tool-call-block-arguments
;; 获取工具调用内容块 record 的 arguments 字段。
;;
;; 语法
;; ----
;; (pi-tool-call-block-arguments obj)
;;
;; 参数
;; ----
;; obj : any?
;; 工具调用内容块 record 实例。
;;
;; 返回值
;; ----
;; alist?
;; arguments 字段的值。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-tool-call-block "tc-001" "read" '(("path" . "foo.txt")) "sig-001")
       ) ;obj-a
       (obj-b (%make-pi-tool-call-block "tc-002" "bash" '(("command" . "ls")) #f))
      ) ;
  (check (pi-tool-call-block-arguments obj-a) => '(("path" . "foo.txt")))
  (check (pi-tool-call-block-arguments obj-b) => '(("command" . "ls")))
) ;let*

(check-report)
