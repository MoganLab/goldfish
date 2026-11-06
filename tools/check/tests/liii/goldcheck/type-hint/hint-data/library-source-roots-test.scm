;; library-source-roots
;; 返回库实现所在的源码根列表，用于跨语言层与标准库层解析 hint 对应的原库路径。
;;
;; 语法
;; ----
;; library-source-roots
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; list of string
;; 源码根相对路径列表，依次为语言层 `goldfish` 与标准库层 `library`。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldcheck type-hint hint-data)
) ;import

(check-set-mode! 'report-failed)

;; 1. 源码根按解析优先级排列
(check library-source-roots => '("goldfish" "library"))

;; 2. 源码根为不含结尾斜杠的相对路径
(check-true
  (every (lambda (root) (not (string-ends? root "/"))) library-source-roots)
) ;check-true

(check-report)
