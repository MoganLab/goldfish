(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi code core compaction serialize)
) ;import

(check-set-mode! 'report-failed)

;; pi-content->text：从消息 content 提取全部文本块的拼接文本
;; content 可以是字符串、JSON 解出的 vector / alist 列表或 pi-block 记录

;; 字符串原样返回
(check (pi-content->text "hello") => "hello")

;; alist 列表：只拼接 type 为 text 的块
(check (pi-content->text '((("type" . "text") ("text" . "a"))
                           (("type" . "toolCall") ("name" . "f"))
                           (("type" . "text") ("text" . "b"))))
  => "ab")

;; JSON 解出的 vector 与空内容
(check (pi-content->text (vector '(("type" . "text") ("text" . "x")))) => "x")
(check (pi-content->text '()) => "")

;; pi-block 记录同样支持
(check (pi-content->text (list (pi-text-block "t1") (pi-text-block "t2"))) => "t1t2")

(check-report)
