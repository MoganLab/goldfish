;; render-container
;; 渲染容器节点（引用、Alert、有序/无序列表、任务项）为行列表。
;;
;; 语法
;; ----
;; (render-container node render-blocks)
;;
;; 参数
;; ----
;; node : pair
;; 容器节点。
;; render-blocks : procedure
;; 块级渲染回调，用于递归渲染容器内子块。
;;
;; 返回值
;; ----
;; list
;; 行字符串列表；容器层只负责自身前缀、空行和缩进。
;;
;; 错误处理
;; ----
;; 容器子节点非法或 item 结构错误时抛出 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown containers))
(import (liii cmark markdown blocks))

(check-set-mode! 'report-failed)

(check-true (procedure? render-container))

;; ---- 引用块 ----
(check (render-container '(blockquote (p "引用")) render-blocks) => '("> 引用"))
(check (render-container '(blockquote (p "a") (p "b")) render-blocks)
  => '("> a" ">" "> b")
) ;check
(check (render-container '(blockquote (blockquote (p "deep"))) render-blocks)
  => '("> > deep")
) ;check
(check (render-container '(blockquote (bullet-list tight (item (p "a")))) render-blocks)
  => '("> - a")
) ;check

;; ---- Alert ----
(check (render-container '(alert note (p "提示")) render-blocks)
  => '("> [!NOTE]" "> 提示")
) ;check
(check (render-container '(alert tip) render-blocks) => '("> [!TIP]"))
(check (render-container '(alert caution (p "a") (p "b")) render-blocks)
  => '("> [!CAUTION]" "> a" ">" "> b")
) ;check

;; ---- 无序列表 ----
(check (render-container '(bullet-list tight (item (p "a")) (item (p "b"))) render-blocks)
  => '("- a" "- b")
) ;check
(check (render-container '(bullet-list loose (item (p "a")) (item (p "b"))) render-blocks)
  => '("- a" "" "- b")
) ;check
;; 嵌套列表缩进
(check
  (render-container
    '(bullet-list tight (item (p "a") (bullet-list tight (item (p "b")))))
    render-blocks
  ) ;render-container
  => '("- a" "  - b")
) ;check
;; 项内多段落（loose）
(check (render-container '(bullet-list loose (item (p "a") (p "b"))) render-blocks)
  => '("- a" "" "  b")
) ;check

;; ---- 有序列表 ----
(check
  (render-container '(ordered-list 1 period tight (item (p "a")) (item (p "b")))
    render-blocks
  ) ;render-container
  => '("1. a" "2. b")
) ;check
(check
  (render-container '(ordered-list 10 one-paren tight (item (p "a")) (item (p "b")))
    render-blocks
  ) ;render-container
  => '("10) a" "11) b")
) ;check
;; 嵌套有序列表缩进
(check
  (render-container
    '(ordered-list 1 period tight
       (item (p "a") (ordered-list 1 one-paren tight (item (p "b")))))
    render-blocks
  ) ;render-container
  => '("1. a" "   1) b")
) ;check

;; ---- 任务列表 ----
(check
  (render-container
    '(bullet-list tight (item checked (p "done")) (item unchecked (p "todo")))
    render-blocks
  ) ;render-container
  => '("- [x] done" "- [ ] todo")
) ;check
(check (render-container '(bullet-list tight (item checked)) render-blocks)
  => '("- [x] ")
) ;check

;; ---- 非法输入 ----
(check-catch 'md-stem-error
  (render-container '(bullet-list tight (p "不是 item")) render-blocks)
) ;check-catch
(check-catch 'md-stem-error (render-container '(unknown-container) render-blocks))

(check-report)
