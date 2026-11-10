;; format-stem-string
;; 在 stem 模式下格式化一段源码文本。
;;
;; 语法
;; ----
;; (format-stem-string source)
;;
;; 参数
;; ----
;; source : string?
;; 待格式化的源码文本。
;;
;; 返回值
;; ----
;; string?
;; 格式化后的文本。
;;
;; 说明
;; ----
;; stem 模式下 quote 与 quasiquote 形式不会被折叠，
;; 因此 `(unquote title)` 之类的模板片段会原样保留。

(import (liii check) (liii goldfmt stem) (srfi srfi-13))

(check-set-mode! 'report-failed)

;; 测试：unquote 形式不会被折叠
(check-true (string-contains (format-stem-string "(document (chapter* (unquote title)))")
              "(unquote title)"
            ) ;string-contains
) ;check-true

;; 测试：quote 形式不会被折叠
(check-true (string-contains (format-stem-string "(document (quote (a b)))")
              "(quote (a b))"
            ) ;string-contains
) ;check-true

;; 测试：不会出现 quasiquote 折叠后的结果
(check-false (string-contains (format-stem-string "(document (chapter* (unquote title)))")
               ",title"
             ) ;string-contains
) ;check-false

(check-report)
