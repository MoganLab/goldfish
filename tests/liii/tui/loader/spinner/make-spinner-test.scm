(import (liii check)
        (liii tui component)
        (liii tui loader spinner))

(check-set-mode! 'report-failed)

;; make-spinner & make-loader 构造与谓词测试

;; 1. 默认构造
(let ((s (make-spinner)))
  (check (spinner? s) => #t)
  (check (loader? s) => #t)
  (check (component? s) => #t)
  (check (spinner-visible? s) => #t)
  (check (spinner-status s) => 'spinning)
  (check (spinner-frame-index s) => 0)
  (check (spinner-text s) => "")
  (check (spinner-prefix s) => "")
  (check (spinner-suffix s) => "")
  (check (spinner-interval s) => 80)
  (check (spinner-current-frame s) => "⠋"))

;; 2. 位置参数传入文本
(let ((s (make-spinner "Loading data...")))
  (check (spinner? s) => #t)
  (check (spinner-text s) => "Loading data...")
  (check (spinner-current-frame s) => "⠋"))

;; 3. 关键字参数配置
(let ((s (make-spinner :frames 'line
                       :text "Searching"
                       :prefix "[app] "
                       :suffix " (0%)"
                       :interval 120
                       :visible #t)))
  (check (spinner-frames s) => '("-" "\\" "|" "/"))
  (check (spinner-text s) => "Searching")
  (check (spinner-prefix s) => "[app] ")
  (check (spinner-suffix s) => " (0%)")
  (check (spinner-interval s) => 120)
  (check (spinner-current-frame s) => "-"))

;; 4. 别名 make-loader 构造
(let ((l (make-loader "Wait" :frames 'star)))
  (check (spinner? l) => #t)
  (check (loader? l) => #t)
  (check (spinner-text l) => "Wait")
  (check (spinner-current-frame l) => "✶"))

;; 5. 初始 visible 为 #f
(let ((s (make-spinner :visible #f)))
  (check (spinner-visible? s) => #f))

;; 6. 类型与错误处理
(check (spinner? "not a spinner") => #f)
(check (spinner? 123) => #f)
(check (loader? '()) => #f)
(check-catch 'type-error (make-spinner :frames 'invalid-frames))
(check-catch 'type-error (make-spinner :interval -10))
(check-catch 'type-error (make-spinner :interval "fast"))

(check-report)
