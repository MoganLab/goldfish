;; make-loader
;; 创建一个加载器（make-spinner 的等价别名）。
;;
;; 语法
;; ----
;; (make-loader [text] [:frames frames] [:text text] [:prefix prefix]
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
;; 加载器组件实例。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui loader spinner)
) ;import

(check-set-mode! 'report-failed)

(let ((l (make-loader "Loading..." :frames 'line)))
  (check-true (component? l))
  (check-true (loader? l))
  (check (spinner-text l) => "Loading...")
  (check (spinner-current-frame l) => "-")
) ;let

(check-report)
