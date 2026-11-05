;; pi-tool-call-block-thought-signature
;; 获取工具调用内容块 record 的 thought-signature 字段。
;;
;; 语法
;; ----
;; (pi-tool-call-block-thought-signature obj)
;;
;; 参数
;; ----
;; obj : any?
;; 工具调用内容块 record 实例。
;;
;; 返回值
;; ----
;; string? 或 #f
;; thought-signature 字段的值。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-tool-call-block "tc-001" "read" '(("path" . "foo.txt")) "sig-001")
       ) ;obj-a
       (obj-b (%make-pi-tool-call-block "tc-002" "bash" '(("command" . "ls")) #f))
      ) ;
  (check (pi-tool-call-block-thought-signature obj-a) => "sig-001")
  (check (pi-tool-call-block-thought-signature obj-b) => #f)
) ;let*

(check-report)
