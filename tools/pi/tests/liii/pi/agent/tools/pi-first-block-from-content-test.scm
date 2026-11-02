;; pi-first-block-from-content
;; 从消息内容 content 中安全提取第一个数据块对象。
;;
;; 语法
;; ----
;; (pi-first-block-from-content c)
;;
;; 参数
;; ----
;; c : (or vector? list? any?)，消息的 content 字段，支持向量或块列表。
;;
;; 返回值
;; ----
;; any?，第一个数据块对象（如 alist 或 pi-block 实例）；若为空或格式不匹配则返回 '()。

(import (scheme base) (liii check) (liii pi ai message) (liii pi agent tools))

(check-set-mode! 'report-failed)

;; 1. 从向量中提取首个块
(let ((vec (vector '((type . "text") (text . "hello")))))
  (check (pi-first-block-from-content vec) => '((type . "text")
                                                (text . "hello")))
) ;let

;; 2. 从列表中提取首个 alist 块
(let ((lst (list '((type . "text") (text . "world")))))
  (check (pi-first-block-from-content lst) => '((type . "text")
                                                (text . "world")))
) ;let

;; 3. 从列表中提取 pi-block record
(let* ((blk (pi-text-block "record text")) (lst (list blk)))
  (check (pi-first-block-from-content lst) => blk)
) ;let*

;; 4. 空向量、空列表或非结构类型返回 '()
(check (pi-first-block-from-content #()) => '())
(check (pi-first-block-from-content '()) => '())
(check (pi-first-block-from-content "string-not-block") => '())

(check-report)
