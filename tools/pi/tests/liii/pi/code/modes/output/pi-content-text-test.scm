;; pi-content-text
;; 提取消息 content 字段中的文本内容。
;;
;; 语法
;; ----
;; (pi-content-text content)
;;
;; 参数
;; ----
;; content : any
;; 消息内容，支持文本块列表与 JSON 解出的向量，非列表内容视为空。
;;
;; 返回值
;; ----
;; string
;; 提取出的文本。

(import (liii check)
  (liii pi ai message)
  (liii pi code modes output)
) ;import

(check-set-mode! 'report-failed)

;; 1. 文本块列表拼接文本
(check (pi-content-text (list (pi-text-block "a") (pi-text-block "b"))) => "ab")

;; 3. 空内容返回空串
(check (pi-content-text '()) => "")

(check-report)
