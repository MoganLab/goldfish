;; make-box
;; 创建边框容器组件，支持多种边框风格、内边距与可选标题。
;;
;; 语法
;; ----
;; (make-box content [:style style] [:title title] [:padding padding]
;;   [:padding-top pt] [:padding-bottom pb] [:padding-left pl] [:padding-right pr])
;;
;; 参数
;; ----
;; content : any
;; 容器承载的子内容，若为字符串则自动包装为 make-text。
;;
;; style : symbol? 或 border-style? (可选，默认为 'single)
;; 边框风格。
;;
;; title : string? (可选)
;; 嵌入顶边框的标题文本。
;;
;; padding : integer? (可选，默认为 0)
;; 上下左右统一内边距。
;;
;; padding-top : integer? (可选)
;; 顶部内边距。
;;
;; padding-bottom : integer? (可选)
;; 底部内边距。
;;
;; padding-left : integer? (可选)
;; 左侧内边距。
;;
;; padding-right : integer? (可选)
;; 右侧内边距。
;;
;; 返回值
;; ----
;; component?
;; 包装为 component 的边框容器实例。

(import (scheme base)
  (liii check)
  (liii tui text-width)
  (liii tui component)
  (liii tui box)
) ;import

(check-set-mode! 'report-failed)

;; 1. 基础创建与 Component 协议检查
(let ((b (make-box "hello" :style 'ascii)))
  (check (component? b) => #t)
  (check (box? b) => #t)
) ;let

;; 2. ASCII 边框渲染
(let* ((b (make-box "hello" :style 'ascii)) (lines (component-render b 9)))
  (check (length lines) => 3)
  (check (car lines) => "+-------+")
  (check (cadr lines) => "|hello  |")
  (check (caddr lines) => "+-------+")
) ;let*

;; 3. Unicode 单线与双线边框
(let* ((b (make-box "test" :style 'single)) (lines (component-render b 8)))
  (check (length lines) => 3)
  (check (display-width (car lines)) => 8)
  (check (car lines) => "┌──────┐")
  (check (cadr lines) => "│test  │")
  (check (caddr lines) => "└──────┘")
) ;let*

;; 4. 标题嵌入
(let* ((b (make-box "ok" :style 'single :title "Hi")) (lines (component-render b 12)))
  (check (car lines) => "┌─ Hi ─────┐")
) ;let*

(check-report)
