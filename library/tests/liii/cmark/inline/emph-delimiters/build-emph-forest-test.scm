;; build-emph-forest
;; 将扁平的强调配对列表构建为嵌套森林结构。
;;
;; 语法
;; ----
;; (build-emph-forest pl delims-v)
;;
;; 参数
;; ----
;; pl : list
;; 配对列表。
;; delims-v : vector
;; 定界符向量。
;;
;; 返回值
;; ----
;; list
;; 返回森林根节点列表。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emph-delimiters))

(check-set-mode! 'report-failed)

(check (build-emph-forest '() #()) => '())

(check-report)
