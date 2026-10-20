(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-tool-call-block
;; 构造 toolCall 类型的 content block record 对象。
(let* ((args '(("path" . "foo.txt")))
       (blk (pi-tool-call-block "tc_001" "read" args :thought-signature "sig123")))
  ;; record 谓词与访问器
  (check-true (pi-tool-call-block? blk))
  (check-true (pi-block? blk))
  (check (pi-block-type blk) => "toolCall")
  (check (pi-tool-call-block-id blk) => "tc_001")
  (check (pi-tool-call-block-name blk) => "read")
  (check (pi-tool-call-block-arguments blk) => args)
  (check (pi-tool-call-block-thought-signature blk) => "sig123")

  ;; 默认无 thoughtSignature
  (let ((blk2 (pi-tool-call-block "tc_002" "bash" '(("command" . "ls")))))
    (check (pi-tool-call-block-thought-signature blk2) => #f))

  ;; 兼容 alist 访问
  (check (cdr (assoc "type" blk)) => "toolCall")
  (check (cdr (assoc 'type blk)) => "toolCall")
  (check (cdr (assoc "id" blk)) => "tc_001")
  (check (cdr (assoc 'id blk)) => "tc_001")
  (check (cdr (assoc "name" blk)) => "read")
  (check (cdr (assoc 'name blk)) => "read")
  (check (cdr (assoc "arguments" blk)) => args)
  (check (cdr (assoc 'arguments blk)) => args)
  (check (cdr (assoc "thoughtSignature" blk)) => "sig123")
  (check (cdr (assoc 'thoughtSignature blk)) => "sig123"))

(check-report)
