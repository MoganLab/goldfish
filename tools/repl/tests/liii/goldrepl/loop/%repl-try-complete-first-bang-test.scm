;; %repl-try-complete-first!
;; 首次触发补全：单候选直接补全，多候选补全公共前缀。
;;
;; 语法
;; ----
;; (%repl-try-complete-first! ed text cur word cands)
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
;; cands : list
;; 补全候选列表。
;;
;; 返回值
;; ----
;; list
;; 三个值：候选列表、候选下标与是否补全成功。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii goldrepl complete)
  (liii goldrepl loop)
) ;import

(check-set-mode! 'report-failed)

;; 没有候选时不补全
(let* ((ed (make-text-input "dd")))
  (let-values (((cands idx ok?) (%repl-try-complete-first! ed "dd" '(0 2) "dd" '())))
    (check cands => '())
    (check idx => 0)
    (check ok? => #f)
  ) ;let-values
) ;let*

;; 单个候选直接补全
(let* ((ed (make-text-input "string-app")) (cands (%repl-completions "string-app")))
  (check-true (pair? cands))
  (let-values (((rest-cands idx ok?)
                (%repl-try-complete-first! ed "string-app" '(0 10) "string-app" cands)
               ) ;
              ) ;
    (check ok? => #t)
    (check (editor-text ed) => "string-append")
  ) ;let-values
) ;let*

;; 多个候选时补全公共前缀
(let* ((ed (make-text-input "string-"))
       (cands '(("string-append" . "") ("string-copy" . "")))
      ) ;
  (let-values (((rest-cands idx ok?)
                (%repl-try-complete-first! ed "string-" '(0 7) "string-" cands)
               ) ;
              ) ;
    (check ok? => #t)
    (check (pair? rest-cands) => #t)
    (check (editor-text ed) => "string-")
  ) ;let-values
) ;let*

(check-report)
