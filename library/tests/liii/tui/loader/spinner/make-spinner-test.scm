;; make-spinner
;; 创建一个微调器（spinner）组件，用于展示异步操作进度或加载状态。
;;
;; 语法
;; ----
;; (make-spinner [text] [:frames frames] [:text text] [:prefix prefix]
;;   [:suffix suffix] [:interval interval] [:visible visible] [:status status]
;;   [:on-change on-change])
;;
;; 参数
;; ----
;; text : string? (可选)
;; 提示文本，默认为 ""。
;;
;; frames : symbol? 或 list? (可选，默认为 'dots)
;; 动画帧样式符号或自定义帧字符串列表。
;;
;; prefix : string? (可选，默认为 "")
;; 位于旋转帧之前的前缀文本。
;;
;; suffix : string? (可选，默认为 "")
;; 位于文本之后的后缀文本。
;;
;; interval : integer? (可选，默认为 80)
;; 建议的刷新间隔（毫秒）。
;;
;; visible : boolean? (可选，默认为 #t)
;; 是否初始可见。
;;
;; status : symbol? (可选，默认为 'spinning)
;; 初始状态符号。
;;
;; on-change : procedure? (可选)
;; 状态发生变化时的回调过程。
;;
;; 返回值
;; ----
;; component?
;; 包装为 component 的微调器组件实例。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui loader spinner)
) ;import

(check-set-mode! 'report-failed)

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
  (check (spinner-current-frame s) => "⠋")
) ;let

;; 2. 位置参数传入文本
(let ((s (make-spinner "Loading data...")))
  (check (spinner? s) => #t)
  (check (spinner-text s) => "Loading data...")
  (check (spinner-current-frame s) => "⠋")
) ;let

;; 3. 关键字参数配置
(let ((s (make-spinner
           :frames   'line
           :text     "Searching"
           :prefix   "[app] "
           :suffix   " (0%)"
           :interval 120
           :visible  #t
         ) ;make-spinner
      ) ;s
     ) ;
  (check (spinner-frames s) => '("-" "\\" "|" "/"))
  (check (spinner-text s) => "Searching")
  (check (spinner-prefix s) => "[app] ")
  (check (spinner-suffix s) => " (0%)")
  (check (spinner-interval s) => 120)
  (check (spinner-current-frame s) => "-")
) ;let

;; 4. 别名 make-loader 构造
(let ((l (make-loader "Wait" :frames 'star)))
  (check (spinner? l) => #t)
  (check (loader? l) => #t)
  (check (spinner-text l) => "Wait")
  (check (spinner-current-frame l) => "✶")
) ;let

;; 5. 初始 visible 为 #f
(let ((s (make-spinner :visible #f)))
  (check (spinner-visible? s) => #f)
) ;let

;; 6. 参数校验
(check-catch 'type-error (make-spinner :text 123))
(check-catch 'type-error (make-spinner :prefix 123))
(check-catch 'type-error (make-spinner :suffix 123))
(check-catch 'type-error (make-spinner :interval -10))
(check-catch 'type-error (make-spinner :interval "80"))
(check-catch 'type-error (make-spinner :on-change "not-a-proc"))

(check-report)
