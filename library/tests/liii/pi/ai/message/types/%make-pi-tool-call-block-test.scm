;; %make-pi-tool-call-block
;; 构造工具调用内容块 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-tool-call-block id name arguments thought-signature)
;;
;; 参数
;; ----
;; id : string?
;; 工具调用唯一标识
;;
;; name : string?
;; 调用的工具名称
;;
;; arguments : alist?
;; 调用参数关联表
;;
;; thought-signature : string? 或 #f
;; 服务商返回的思考签名，缺失时为 #f
;;
;; 返回值
;; ----
;; pi-tool-call-block?
;; 工具调用内容块 record 实例。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-tool-call-block "tc-001" "read" '(("path" . "foo.txt")) "sig-001")
      ) ;obj
     ) ;
  (check-true (pi-tool-call-block? obj))
  (check (pi-tool-call-block-id obj) => "tc-001")
  (check (pi-tool-call-block-name obj) => "read")
  (check (pi-tool-call-block-arguments obj) => '(("path" . "foo.txt")))
  (check (pi-tool-call-block-thought-signature obj) => "sig-001")
) ;let

(check-report)
