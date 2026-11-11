;; pi-boundary-message-content
;; 将消息记录、向量或 alist 中的 content 统一为内容块列表。
;;
;; 语法
;; ----
;; (pi-boundary-message-content message)
;;
;; 参数
;; ----
;; message : any
;; 用户、assistant、tool result 消息记录或 alist。
;;
;; 返回值
;; ----
;; list
;; 内容块列表；字符串包装为单元素列表，无内容时为空列表。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi code all-in-one boundary)
) ;import

(check-set-mode! 'report-failed)
(let ((block (pi-text-block "ok")))
  (check (pi-boundary-message-content (pi-assistant-message (list block)))
    =>
    (list block)
  ) ;check
  (check (pi-boundary-message-content (pi-user-message (vector block)))
    =>
    (list block)
  ) ;check
  (check (pi-boundary-message-content '((content . "raw"))) => '("raw"))
) ;let
(check (pi-boundary-message-content (pi-user-message "hi")) => '("hi"))
(check (pi-boundary-message-content #f) => '())

(check-report)
