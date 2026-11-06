;; make-truncated-text
;; 创建单行截断文本组件。
;;
;; 语法
;; ----
;; (make-truncated-text str [ellipsis])
;;
;; 参数
;; ----
;; str : string
;; 要显示的文本内容。
;; ellipsis : string, 可选
;; 超出宽度时附加的省略标记，默认为 "..."。
;;
;; 返回值
;; ----
;; component
;; 保证渲染为单行的截断文本组件。
;;
;; 说明
;; ----
;; 1. 始终只输出单行。若原串含有 \n，以首行作为基准截断；
;; 2. 当字符串长度 <= width 时，原样输出；
;; 3. 当字符串长度 > width 时，截取前缀并附加 ellipsis，使得总长度等于 width；
;; 4. 若 width < ellipsis 长度，安全降级截取 ellipsis 自身的前 width 个字符；
;; 5. 若 width <= 0，返回 '("")；
;; 6. 若 str 或 ellipsis 类型非法，抛出 type-error。

(import (liii check) (liii tui component))

(check-set-mode! 'report-failed)

;; 未超出宽度
(let ((c (make-truncated-text "hello")))
  (check (component-render c 10) => '("hello"))
  (check (component-render c 5) => '("hello"))
) ;let

;; 超出宽度截断
(let ((c (make-truncated-text "hello world")))
  (check (component-render c 8) => '("hello..."))
  (check (component-render c 6) => '("hel..."))
) ;let

;; 自定义省略符
(let ((c (make-truncated-text "hello world" " ~")))
  (check (component-render c 7) => '("hello ~"))
) ;let

;; 极小宽度测试
(let ((c (make-truncated-text "hello world")))
  (check (component-render c 3) => '("..."))
  (check (component-render c 2) => '(".."))
  (check (component-render c 1) => '("."))
  (check (component-render c 0) => '(""))
  (check (component-render c -1) => '(""))
) ;let

;; 包含换行符（只取第一行）
(let ((c (make-truncated-text "first line\nsecond line")))
  (check (component-render c 20) => '("first line"))
  (check (component-render c 8) => '("first..."))
) ;let

;; 错误处理
(check-catch 'type-error (make-truncated-text 123))
(check-catch 'type-error (make-truncated-text "hello" 123))

(check-report)
