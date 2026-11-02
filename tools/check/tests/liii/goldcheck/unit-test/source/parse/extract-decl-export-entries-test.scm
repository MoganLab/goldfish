;; extract-decl-export-entries
;; 从库 export 声明中提取外部名称与内部名称的点对列表并追加到累加器。
;;
;; 语法
;; ----
;; (extract-decl-export-entries decl acc)
;;
;; 参数
;; ----
;; decl : pair
;; 库声明表单，如 (export foo (rename internal external))。
;; acc : list
;; 累加器列表。
;;
;; 返回值
;; ------
;; list of pair
;; 包含 (外部名称 . 内部名称) 点对的列表（逆序追加到 acc）。

(import (scheme base)
  (liii check)
  (liii goldcheck unit-test source parse)
) ;import

(check-set-mode! 'report-failed)

;; 1. 非 export 声明直接返回原累加器
(check (extract-decl-export-entries '(import (scheme base)) '()) => '())

;; 2. 普通符号导出条目提取
(check (extract-decl-export-entries '(export foo bar) '())
  =>
  '(("bar" . "bar") ("foo" . "foo"))
) ;check

;; 3. rename 形式导出条目提取（外部名称在前，内部名称在后）
(check
  (extract-decl-export-entries '(export (rename orig-name alias-name)) '())
  =>
  '(("alias-name" . "orig-name"))
) ;check

;; 4. 混合导出与保留已有累加器
(check
  (extract-decl-export-entries '(export a (rename b c)) '(("old" . "old")))
  =>
  '(("c" . "b") ("a" . "a") ("old" . "old"))
) ;check

(check-report)
