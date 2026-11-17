
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

;; make-cmark-table
;; 创建表格节点，并保留对齐、表头和正文行信息。
;;
;; 语法
;; ----
;; (make-cmark-table alignments header rows)

(let ((node (make-cmark-table '(default right) '(("Name" "Value")) '(("A" "1")
                                                                     ("B" "2")))
      ) ;node
     ) ;
  (check (cmark-node-type node) => 'table)
  (check (cdr (assoc 'alignments (cmark-node-data node))) => '(default right))
  (check (cdr (assoc 'header (cmark-node-data node))) => '(("Name" "Value")))
  (check (cdr (assoc 'rows (cmark-node-data node))) => '(("A" "1") ("B" "2")))
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试空行列表
(let ((node (make-cmark-table '() '() '())))
  (check (cmark-node-type node) => 'table)
  (check (cdr (assoc 'alignments (cmark-node-data node))) => '())
  (check (cdr (assoc 'header (cmark-node-data node))) => '())
  (check (cdr (assoc 'rows (cmark-node-data node))) => '())
) ;let

;; 测试单列单行
(let ((node (make-cmark-table '(left) '(("Header")) '(("Cell")))))
  (check (cdr (assoc 'alignments (cmark-node-data node))) => '(left))
  (check (cdr (assoc 'header (cmark-node-data node))) => '(("Header")))
  (check (cdr (assoc 'rows (cmark-node-data node))) => '(("Cell")))
) ;let

;; 测试超多列
(let ((node (make-cmark-table '(left center right default)
              '(("A" "B" "C" "D"))
              '(("1" "2" "3" "4") ("5" "6" "7" "8"))
            ) ;make-cmark-table
      ) ;node
     ) ;
  (check (cdr (assoc 'alignments (cmark-node-data node)))
    =>
    '(left center right default)
  ) ;check
  (check
    (length (cdr (assoc 'header (cmark-node-data node))))
    =>
    1
  ) ;check
  (check
    (length (cdr (assoc 'rows (cmark-node-data node))))
    =>
    2
  ) ;check
) ;let

;; 测试只有表头无表体
(let ((node (make-cmark-table '(center) '(("OnlyHeader")) '())))
  (check (cdr (assoc 'rows (cmark-node-data node))) => '())
) ;let

(check-report)
