;; pi-text-block
;; 构造 text 类型的 content block record 对象。
;;
;; 语法
;; ----
;; (pi-text-block text)
;;
;; 参数
;; ----
;; text : string?
;; 文本块正文。
;;
;; 返回值
;; ----
;; pi-text-block?
;; 文本块 record 对象。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(let* ((blk (pi-text-block "Hello, world!")))
  (check-true (pi-text-block? blk))
  (check-true (pi-block? blk))
  (check (pi-block-type blk) => "text")
  (check (pi-text-block-text blk) => "Hello, world!")
) ;let*

(check-report)
