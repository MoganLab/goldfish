;; %repl-apply-completion!
;; 用补全候选替换光标处的当前词并移动光标。
;;
;; 语法
;; ----
;; (%repl-apply-completion! ed text cur word repl)
;;
;; 参数
;; ----
;; ed : editor
;; 编辑器对象。
;; text : string
;; 编辑器中的当前文本。
;; cur : list
;; 光标位置，形如 `(行号 . 列号)`。
;; word : string
;; 光标左侧的当前词。
;; repl : string
;; 用于替换当前词的补全候选。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未定义，函数通过修改编辑器生效。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii goldrepl complete)
  (liii goldrepl loop)
) ;import

(check-set-mode! 'report-failed)

;; 替换中间的词，并把光标移动到新词之后
(let* ((ed (make-text-input "(string-> 123)"))
       (txt (editor-text ed))
       (cur '(0 9))
       (w (%repl-word-at txt (cadr cur)))
      ) ;
  (%repl-apply-completion! ed txt cur w "string->list")
  (check (editor-text ed) => "(string->list 123)")
  (check (editor-cursor ed) => '(0 13))
) ;let*

;; 替换行尾的词
(let* ((ed (make-text-input "string-"))
       (txt (editor-text ed))
       (cur '(0 7))
       (w (%repl-word-at txt (cadr cur)))
      ) ;
  (%repl-apply-completion! ed txt cur w "string-append")
  (check (editor-text ed) => "string-append")
  (check (editor-cursor ed) => '(0 13))
) ;let*

(check-report)
