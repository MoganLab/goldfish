;; %repl-submit-meta!
;; 提交以逗号开头的元命令，返回 (退出标志 . 历史条目)。
;;
;; 语法
;; ----
;; (%repl-submit-meta! ed text hist app-out! eval-show!)
;;
;; 参数
;; ----
;; ed : editor
;; 编辑器对象。
;; text : string
;; 待提交的元命令文本。
;; hist : history
;; 编辑器的历史对象。
;; app-out! : procedure
;; 输出一行文本的回调，接收文本行列表。
;; eval-show! : procedure
;; 执行求值并展示结果的回调，接收代码文本。
;;
;; 返回值
;; ----
;; list
;; 两个值：退出标志与新增的历史条目列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii goldrepl loop)
) ;import

(check-set-mode! 'report-failed)

;; 提交帮助命令：输出帮助内容，清空输入并记录历史
(let* ((ed (make-text-input ",help"))
       (hist (editor-history ed))
       (lines '())
       (evaluated '())
      ) ;
  (let-values (((q entries)
                (%repl-submit-meta! ed
                  ",help"
                  hist
                  (lambda (l) (set! lines (append lines l)))
                  (lambda (c) (set! evaluated (append evaluated (list c))))
                ) ;%repl-submit-meta!
               ) ;
              ) ;

    (check q => #f)
    (check entries => '(",help"))
    ;; 输入框被清空
    (check (editor-text ed) => "")
    ;; 历史中记录了本次提交
    (check (any (lambda (e) (string=? e ",help")) (history-entries hist)) => #t)
    ;; 帮助内容被输出
    (check-true (any (lambda (l) (string-contains l "Meta commands:")) lines))
    ;; 帮助命令不需要执行求值
    (check evaluated => '())
  ) ;let-values
) ;let*

;; 提交退出命令：返回退出标志
(let* ((ed (make-text-input ",quit")) (hist (editor-history ed)))
  (let-values (((q entries)
                (%repl-submit-meta! ed ",quit" hist (lambda (l) #f) (lambda (c) #f))
               ) ;
              ) ;

    (check q => #t)
    (check entries => '(",quit"))
  ) ;let-values
) ;let*

(check-report)
