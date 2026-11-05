;; pi-text-block
;; 构造 text 类型的 content block record 对象。
;;
;; 语法
;; ----
;; (pi-text-block text [signature])
;;
;; 参数
;; ----
;; text : string?
;; 文本块正文。
;; signature : (or string? #f)
;; 可选。text 块签名（对应上游 pi-ai 的 TextContent.textSignature），
;; 用于 Gemini 等 provider 的 thought signature 回放。
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

;; 可选签名参数
(let* ((blk (pi-text-block "hi" "QUJD")))
  (check (pi-text-block-text blk) => "hi")
  (check (pi-text-block-signature blk) => "QUJD")
) ;let*

;; 省略签名时默认为 #f
(check (pi-text-block-signature (pi-text-block "hi")) => #f)

(check-report)
