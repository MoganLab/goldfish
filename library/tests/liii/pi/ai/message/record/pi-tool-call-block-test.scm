;; pi-tool-call-block
;; 构造 toolCall 类型的 content block record 对象。
;;
;; 语法
;; ----
;; (pi-tool-call-block id name arguments)
;; (pi-tool-call-block id name arguments :thought-signature sig)
;;
;; 参数
;; ----
;; id : string?
;; 工具调用唯一标识。
;;
;; name : string?
;; 调用的工具名。
;;
;; arguments : alist?
;; 调用参数关联表。
;;
;; 返回值
;; ----
;; pi-tool-call-block?
;; 工具调用块 record 对象。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(let* ((args '(("path" . "foo.txt")))
       (blk (pi-tool-call-block "tc_001" "read" args :thought-signature "sig123"))
      ) ;
  (check-true (pi-tool-call-block? blk))
  (check-true (pi-block? blk))
  (check (pi-block-type blk) => "toolCall")
  (check (pi-tool-call-block-id blk) => "tc_001")
  (check (pi-tool-call-block-name blk) => "read")
  (check (pi-tool-call-block-arguments blk) => args)
  (check (pi-tool-call-block-thought-signature blk) => "sig123")

  (let ((blk2 (pi-tool-call-block "tc_002" "bash" '(("command" . "ls")))))
    (check (pi-tool-call-block-thought-signature blk2) => #f)
  ) ;let
) ;let*

(check-report)
