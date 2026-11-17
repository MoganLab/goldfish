(import (liii check)
  (liii cmark block parser)
  (liii cmark node)
  (liii cmark block alert)
) ;import

(check-set-mode! 'report-failed)

;; parse-alert-block
;; 解析 `> [!NOTE]` 这类 GFM / admonition 风格告警块。
;;
;; 语法
;; ----
;; (parse-alert-block kind lines)

(check-true (alert-block-start? "> [!NOTE]"))
(check-true (alert-block-start? ">[!warning]"))
(check-false (alert-block-start? "> [!UNKNOWN]"))
(check-false (alert-block-start? "> 普通引用"))

(let ((parsed (collect-alert-block-lines '("> [!TIP] 标题" "> 第二行" ""
                                           "tail"))
      ) ;parsed
     ) ;
  (check (car parsed) => '((kind . tip) (lines "标题" "第二行")))
  (check (cdr parsed) => '("" "tail"))
) ;let

(let ((node (parse-alert-block 'note '("第一行" "" "第二行"))))
  (check (cmark-node-type node) => 'alert)
  (check (assoc 'kind (cmark-node-data node)) => '(kind . note))
  (check (assoc 'content (cmark-node-data node))
    =>
    '(content . "第一行\n\n第二行")
  ) ;check
) ;let

(check-report)
