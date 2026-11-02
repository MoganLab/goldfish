(import (liii check)
        (liii tui loader spinner))

(check-set-mode! 'report-failed)

;; spinner-text 文本、前缀与后缀动态更新测试

;; 1. 主体文本动态修改
(let ((s (make-spinner "Initial")))
  (check (spinner-text s) => "Initial")
  (spinner-set-text! s "Updated message")
  (check (spinner-text s) => "Updated message")
  (spinner-set-text! s "")
  (check (spinner-text s) => ""))

;; 2. 前缀动态修改
(let ((s (make-spinner "Task" :prefix ">> ")))
  (check (spinner-prefix s) => ">> ")
  (spinner-set-prefix! s "[INFO] ")
  (check (spinner-prefix s) => "[INFO] ")
  (spinner-set-prefix! s "")
  (check (spinner-prefix s) => ""))

;; 3. 后缀动态修改
(let ((s (make-spinner "Downloading" :suffix " 10%")))
  (check (spinner-suffix s) => " 10%")
  (spinner-set-suffix! s " 50%")
  (check (spinner-suffix s) => " 50%")
  (spinner-set-suffix! s "")
  (check (spinner-suffix s) => ""))

;; 4. 间隔设置与修改
(let ((s (make-spinner :interval 100)))
  (check (spinner-interval s) => 100)
  (spinner-set-interval! s 50)
  (check (spinner-interval s) => 50))

;; 5. 异常类型拦截
(let ((s (make-spinner)))
  (check-catch 'type-error (spinner-set-text! s 123))
  (check-catch 'type-error (spinner-set-prefix! s #f))
  (check-catch 'type-error (spinner-set-suffix! s 'bad))
  (check-catch 'type-error (spinner-set-interval! s -1))
  (check-catch 'type-error (spinner-set-interval! s "fast")))

(check-report)
