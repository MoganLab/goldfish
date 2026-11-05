;; anthropic-system-blocks
;; 将 system 提示词转为挂 cache_control 断点的 content block 数组。
;;
;; 语法
;; ----
;; (anthropic-system-blocks system-prompt)
;;
;; 参数
;; ----
;; system-prompt : string
;; 系统提示词文本。
;;
;; 返回值
;; ----
;; vector
;; 长度为 1 的 content block 数组，块含 type/text/cache_control 字段，
;; cache_control 为 {"type": "ephemeral"} 断点。

(import (liii check)
  (liii pi ai api anthropic-messages cache)
) ;import

(check-set-mode! 'report-failed)

(let ((blocks (anthropic-system-blocks "You are a helpful assistant")))
  (check (vector? blocks) => #t)
  (check (vector-length blocks) => 1)
  (let ((blk (vector-ref blocks 0)))
    (check (cdr (assoc "type" blk)) => "text")
    (check (cdr (assoc "text" blk)) => "You are a helpful assistant")
    (check (cdr (assoc "cache_control" blk)) => '(("type" . "ephemeral")))
  ) ;let
) ;let

;; 多行提示词原样保留
(let ((blocks (anthropic-system-blocks "line1\nline2")))
  (check (cdr (assoc "text" (vector-ref blocks 0))) => "line1\nline2"))

(check-report)
