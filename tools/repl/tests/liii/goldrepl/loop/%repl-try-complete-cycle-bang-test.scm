;; %repl-try-complete-cycle!
;; 循环切换补全候选。
;;
;; 语法
;; ----
;; (%repl-try-complete-cycle! ed text cur word cands idx)
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
;; idx : integer
;; 当前候选下标。
;;
;; 返回值
;; ----
;; list
;; 三个值：候选列表、下一个候选下标与是否补全成功。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii goldrepl loop)
) ;import

(check-set-mode! 'report-failed)

;; 选中第一个候选之后下标前进到 1
(let* ((ed (make-text-input "string-"))
       (cands '(("string-append" . "") ("string-copy" . "")))
      ) ;
  (let-values (((rest-cands idx ok?)
                (%repl-try-complete-cycle! ed "string-" '(0 7) "string-" cands 0)
               ) ;
              ) ;
    (check ok? => #t)
    (check idx => 1)
    (check (editor-text ed) => "string-append")
  ) ;let-values
) ;let*

;; 下标在末尾时回绕到 0
(let* ((ed (make-text-input "string-"))
       (cands '(("string-append" . "") ("string-copy" . "")))
      ) ;
  (let-values (((rest-cands idx ok?)
                (%repl-try-complete-cycle! ed "string-" '(0 7) "string-" cands 1)
               ) ;
              ) ;
    (check ok? => #t)
    (check idx => 0)
    (check (editor-text ed) => "string-copy")
  ) ;let-values
) ;let*

(check-report)
