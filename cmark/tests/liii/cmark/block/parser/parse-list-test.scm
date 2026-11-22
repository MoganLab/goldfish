;; parse-list
;; 解析 Markdown 列表与列表项。
;;
;; 语法
;; ----
;; (parse-list lines)
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
;; unordered-list-item?、ordered-list-item? 和 parse-list-item
;; 一起负责判断与拆解列表项头部。

(import (liii check) (liii cmark block parser) (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (unordered-list-item? "- item"))
(check-true (ordered-list-item? "1. item"))
(check-true (ordered-list-item? "10. item"))
(check-true (ordered-list-item? "1) item"))
(check-false (unordered-list-item? "\\- item"))
(check-false (ordered-list-item? "1\\. item"))
(check-false (ordered-list-item? "1\\) item"))

(let ((item (parse-list-item "- Hello")))
  (check (car item) => 'bullet)
  (check (cdr item) => "Hello")
) ;let

(let ((item (parse-list-item "10. Hello")))
  (check (car (car item)) => 'ordered)
  (check (cdr (car item)) => 10)
  (check (cdr item) => "Hello")
) ;let

(check (ordered-list-delimiter "1. Hello") => 'period)
(check (ordered-list-delimiter "1) Hello") => 'one-paren)
(check (task-list-item-status "[ ] Hello") => 'unchecked)
(check (task-list-item-status "[x] Hello") => 'checked)
(check (task-list-item-status "[X] Hello") => 'checked)
(check-false (task-list-item-status "[x]Hello"))
(check (strip-task-list-item-marker "[x] Hello") => "Hello")

(let ((list-node (parse-list '("- Item 1" "- Item 2"))))
  (check (cmark-node-type list-node) => 'list)
  (check (length (cmark-node-children list-node)) => 2)
) ;let

(let ((list-node (parse-list '("* Item 1" "* Item 2"))))
  (check (cmark-node-type list-node) => 'list)
) ;let

(let ((list-node (parse-list '("1. First" "2. Second"))))
  (check (cmark-node-type list-node) => 'list)
) ;let

(let ((list-node (parse-list '("3. Third" "4. Fourth"))))
  (check (cdr (assoc 'type (cmark-node-data list-node))) => 'ordered)
  (check (cdr (assoc 'start (cmark-node-data list-node))) => 3)
  (check (cdr (assoc 'delimiter (cmark-node-data list-node))) => 'period)
) ;let

(let ((list-node (parse-list '("1) First" "2) Second"))))
  (check (cdr (assoc 'type (cmark-node-data list-node))) => 'ordered)
  (check (cdr (assoc 'start (cmark-node-data list-node))) => 1)
  (check (cdr (assoc 'delimiter (cmark-node-data list-node))) => 'one-paren)
) ;let

(let* ((list-node (parse-list '("- [ ] Todo" "- [x] Done")))
       (items (cmark-node-children list-node))
      ) ;
  (check
    (cdr (assoc 'task-status (cmark-node-data (car items))))
    =>
    'unchecked
  ) ;check
  (check
    (cdr (assoc 'content (cmark-node-data (car items))))
    =>
    "Todo"
  ) ;check
  (check
    (cdr (assoc 'task-status (cmark-node-data (cadr items))))
    =>
    'checked
  ) ;check
  (check
    (cdr (assoc 'content (cmark-node-data (cadr items))))
    =>
    "Done"
  ) ;check
) ;let*

(check-report)
