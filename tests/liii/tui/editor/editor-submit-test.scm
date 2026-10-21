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

(check-report)
