;; parse-blockquote
;; 解析块引用行序列。
;;
;; 语法
;; ----
;; (parse-blockquote lines)
;;
;;
;; 参数
;; ----
;; lines : any
;; 输入参数。
;;
;; 返回值
;; ----
;; cmark-node 或 pair 或 #f
;; 返回解析得到的节点或结构。
;; 描述
;; ----
;; blockquote-line? 用于判断一行是否以 > 开头。

(import (liii check) (liii cmark block parser) (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (blockquote-line? "> quote"))
(check-false (blockquote-line? "\\> quote"))

(let ((blockquote (parse-blockquote '("> Quote line 1" "> Quote line 2"))))
  (check (cmark-node-type blockquote) => 'block_quote)
) ;let

(let ((blockquote (parse-blockquote '(">Quote without space"))))
  (check (cmark-node-type blockquote) => 'block_quote)
) ;let

(let ((blockquote (parse-blockquote '("> This is a quote."
                                      "  Continued line"
                                      "> Another line"))
      ) ;blockquote
     ) ;
  (check (cmark-node-type blockquote) => 'block_quote)
  (check (cdr (assoc 'content (cmark-node-data blockquote)))
    =>
    "This is a quote.\n  Continued line\nAnother line"
  ) ;check
) ;let

(check-report)
