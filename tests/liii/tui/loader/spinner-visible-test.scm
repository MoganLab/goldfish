(import (liii check)
        (liii tui component)
        (liii tui loader spinner))

(check-set-mode! 'report-failed)

;; spinner-visible 显隐切换测试

;; 1. 默认可见与手动隐藏
(let ((s (make-spinner "Working")))
  (check (spinner-visible? s) => #t)
  (check (pair? (component-render s 80)) => #t)
  ;; 隐藏
  (spinner-hide! s)
  (check (spinner-visible? s) => #f)
  (check (component-render s 80) => '())
  ;; 重新显示
  (spinner-show! s)
  (check (spinner-visible? s) => #t)
  (check (pair? (component-render s 80)) => #t))

;; 2. spinner-set-visible!
(let ((s (make-spinner "Job" :visible #f)))
  (check (spinner-visible? s) => #f)
  (check (component-render s 80) => '())
  (spinner-set-visible! s #t)
  (check (spinner-visible? s) => #t)
  (check (pair? (component-render s 80)) => #t)
  (spinner-set-visible! s #f)
  (check (spinner-visible? s) => #f))

;; 3. on-change 回调在显隐改变时触发
(let* ((count 0)
       (s (make-spinner "Job"
                        :on-change (lambda (sp) (set! count (+ count 1))))))
  (spinner-hide! s)
  (check count => 1)
  (spinner-show! s)
  (check count => 2))

(check-report)
