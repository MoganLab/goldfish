(import (liii check)
  (liii string)
  (liii pi ai message)
  (liii pi code core compaction fileops)
  (liii pi code core compaction)
) ;import

(check-set-mode! 'report-failed)

;; pi-extract-file-ops
;; 从助手消息的工具调用中提取文件读取、写入和编辑路径。
;;
;; 语法
;; ----
;; (pi-extract-file-ops messages) -> alist
;;
;; 参数
;; ----
;; messages : list
;; 会话消息列表；仅检查 assistant 消息中的 read、write、edit 工具调用。
;;
;; 返回值
;; ----
;; alist
;; 包含 read、written、edited 三个键，值为按出现顺序排列且不重复的路径列表。
;;
;; 示例
;; ----
;; (pi-extract-file-ops messages)
;; => '((read . ("/a")) (written . ("/b")) (edited . ("/c")))

(define (tc name path)
  (pi-tool-call-block (string-append "id-" name)
    name
    (list (cons "path" path))
    #f
  ) ;pi-tool-call-block
) ;define

(let* ((msgs
         (list (pi-assistant-message (list (tc "read" "/a") (tc "write" "/b") (tc "edit" "/c"))
               ) ;pi-assistant-message
           (pi-assistant-message (list (tc "read" "/d") (tc "read" "/a")))
         ) ;list
       ) ;msgs
       (ops (pi-extract-file-ops msgs))
      ) ;
  (check (cdr (assoc 'read ops)) => '("/a" "/d"))
  (check (cdr (assoc 'written ops)) => '("/b"))
  (check (cdr (assoc 'edited ops)) => '("/c"))
) ;let*

(check-report)
