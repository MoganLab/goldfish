(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-text-block
;; 构造 text 类型的 content block record 对象。
(let* ((blk (pi-text-block "Hello, world!")))
  ;; record 谓词与访问器
  (check-true (pi-text-block? blk))
  (check-true (pi-block? blk))
  (check (pi-block-type blk) => "text")
  (check (pi-text-block-text blk) => "Hello, world!")

  ;; 兼容 alist 访问
  (check (cdr (assoc "type" blk)) => "text")
  (check (cdr (assoc 'type blk)) => "text")
  (check (cdr (assoc "text" blk)) => "Hello, world!")
  (check (cdr (assoc 'text blk)) => "Hello, world!"))

(check-report)
