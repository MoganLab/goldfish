(import (scheme base)
  (liii check)
  (liii tui editor)
  (liii tui component)
) ;import

(check-set-mode! 'report)

;; 测试 on-submit 在按 Enter 时的回调
(let* ((submitted-text #f)
       (ed (make-editor "hello" :multiline #f :on-submit (lambda (comp text)
                                                           (set! submitted-text text)
                                                         ) ;lambda
           ) ;make-editor
       ) ;ed
      ) ;let*
  (check (component-handle-input ed 'enter) => #t)
  (check submitted-text => "hello")
) ;let*

;; submit-on-enter? 显式开启：即使 multiline 为真，Enter 也提交而不是换行
(let* ((submitted-text #f)
       (ed (make-editor "hello" :multiline #t :submit-on-enter? #t
             :on-submit (lambda (comp text)
                          (set! submitted-text text)
                        ) ;lambda
           ) ;make-editor
       ) ;ed
      ) ;let*
  (check (component-handle-input ed 'enter) => #t)
  (check submitted-text => "hello")
  (check (editor-text ed) => "hello")
) ;let*

;; 显式关闭 submit-on-enter? 时，即使多行模式 Enter 也不提交、不换行
(let* ((submitted-text #f)
       (ed (make-editor "hello" :multiline #t :submit-on-enter? #f
             :on-submit (lambda (comp text)
                          (set! submitted-text text)
                        ) ;lambda
           ) ;make-editor
       ) ;ed
      ) ;let*
  (check (component-handle-input ed 'enter) => #f)
  (check submitted-text => #f)
  (check (editor-text ed) => "hello")
) ;let*

;; 显式关闭 submit-on-enter? 时，单行模式 Enter 不提交
(let* ((submitted-text #f)
       (ed (make-editor "hello" :multiline #f :submit-on-enter? #f
             :on-submit (lambda (comp text)
                          (set! submitted-text text)
                        ) ;lambda
           ) ;make-editor
       ) ;ed
      ) ;let*
  (check (component-handle-input ed 'enter) => #f)
  (check submitted-text => #f)
) ;let*

(check-report)
