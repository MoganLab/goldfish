;; %repl-submit-code!
;; 提交代码，括号未闭合时继续累积多行输入。
;;
;; 语法
;; ----
;; (%repl-submit-code! ed text buf hist app-out! eval-show! atom?)
;;
;; 参数
;; ----
;; ed : editor
;; 编辑器对象。
;; text : string
;; 待提交的代码文本。
;; buf : list
;; 累积的多行输入缓冲区。
;; hist : history
;; 编辑器的历史对象。
;; app-out! : procedure
;; 输出一行文本的回调，接收文本行列表。
;; eval-show! : procedure
;; 执行求值并展示结果的回调，接收代码文本。
;; atom? : procedure
;; 判断文本是否为原子表达式的回调。
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

;; 完整的表达式会被求值，并记录到历史
(let* ((ed (make-text-input "(+ 1 2)"))
       (hist (editor-history ed))
       (buf (list ""))
       (lines '())
       (evaluated '())
      ) ;
  (let-values (((q entries)
                (%repl-submit-code! ed
                  "(+ 1 2)"
                  buf
                  hist
                  (lambda (l) (set! lines (append lines l)))
                  (lambda (c) (set! evaluated (append evaluated (list c))))
                  %repl-atom?
                ) ;%repl-submit-code!
               ) ;
              ) ;

    (check q => #f)
    (check entries => '("(+ 1 2)"))
    ;; 输入框与缓冲区都被清空
    (check (editor-text ed) => "")
    (check (car buf) => "")
    ;; 代码被求值
    (check evaluated => '("(+ 1 2)"))
    ;; 历史中记录了本次提交
    (check (any (lambda (e) (string=? e "(+ 1 2)")) (history-entries hist)) => #t)
  ) ;let-values
) ;let*

;; 括号未闭合时继续累积输入，不求值
(let* ((ed (make-text-input "(+ 1"))
       (hist (editor-history ed))
       (buf (list ""))
       (evaluated '())
      ) ;
  (let-values (((q entries)
                (%repl-submit-code! ed
                  "(+ 1"
                  buf
                  hist
                  (lambda (l) #f)
                  (lambda (c) (set! evaluated (append evaluated (list c))))
                  %repl-atom?
                ) ;%repl-submit-code!
               ) ;
              ) ;

    (check q => #f)
    (check entries => '())
    ;; 缓冲区中保留了未完成的输入
    (check (car buf) => '("(+ 1"))
    ;; 输入框被清空，等待下一行
    (check (editor-text ed) => "")
    ;; 未完成的输入不会被求值
    (check evaluated => '())
  ) ;let-values
) ;let*

(check-report)
