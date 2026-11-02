;; extract-decl-imports
;; 从库声明表单中解析提取 import 的库标识符规范列表并追加到累加器。
;;
;; 语法
;; ----
;; (extract-decl-imports decl acc)
;;
;; 参数
;; ----
;; decl : pair
;; 库声明表单，如 (import (scheme base) (only (liii string) string-join))。
;; acc : list
;; 累加器列表。
;;
;; 返回值
;; ------
;; list of list
;; 解析得到的库标识符规范列表（逆序追加到 acc）。

(import (scheme base)
  (liii check)
  (liii goldcheck unit-test source parse)
) ;import

(check-set-mode! 'report-failed)

;; 1. 非 import 声明返回原累加器
(check (extract-decl-imports '(export a b) '()) => '())

;; 2. 解析普通 import 库声明
(check (extract-decl-imports '(import (scheme base) (liii path)) '())
  =>
  '((liii path) (scheme base))
) ;check

;; 3. 支持 only, prefix, except, rename 等修饰符
(check
  (extract-decl-imports
    '(import (only (liii string) string-split) (prefix (liii math) m:))
    '()
  ) ;extract-decl-imports
  =>
  '((liii math) (liii string))
) ;check

;; 4. 保留已有累加器内容
(check (extract-decl-imports '(import (scheme file)) '((liii base)))
  =>
  '((scheme file) (liii base))
) ;check

(check-report)
