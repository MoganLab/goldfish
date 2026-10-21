(import (liii check)
        (liii tui editor view))

(check-set-mode! 'report-failed)

;; render-editor-lines & render-line-with-cursor 渲染视图测试

;; 1. render-line-with-cursor 未聚焦时原样返回
(check (render-line-with-cursor "hello" 2 #f 'inverse) => "hello")

;; 2. render-line-with-cursor 聚焦反显光标 (行中)
(check (render-line-with-cursor "hello" 1 #t 'inverse)
  => "h\x1b[7me\x1b[0mllo")

;; 3. render-line-with-cursor 聚焦反显光标 (行尾追加空格反显)
(check (render-line-with-cursor "hello" 5 #t 'inverse)
  => "hello\x1b[7m \x1b[0m")

;; 4. render-line-with-cursor 竖线光标样式
(check (render-line-with-cursor "hello" 2 #t 'bar)
  => "he|llo")

;; 5. 空内容且未聚焦显示 placeholder
(check (render-editor-lines '("") 0 0 #f "Type here..." 40 'inverse)
  => '("Type here..."))

;; 6. 空内容但聚焦时显示光标
(check (render-editor-lines '("") 0 0 #t "Type here..." 40 'inverse)
  => '("\x1b[7m \x1b[0m"))

;; 7. 多行渲染，仅光标所在行绘制光标
(let ((lines '("line 1" "line 2")))
  (check (render-editor-lines lines 1 0 #t "" 40 'inverse)
    => '("line 1" "\x1b[7ml\x1b[0mine 2")))

;; 8. 包含中文字符的反显与竖线光标样式
(check (render-line-with-cursor "你好世界" 1 #t 'inverse)
  => "你\x1b[7m好\x1b[0m世界")
(check (render-line-with-cursor "你好世界" 1 #t 'bar)
  => "你|好世界")
(check (render-line-with-cursor "你好" 2 #t 'inverse)
  => "你好\x1b[7m \x1b[0m")

(check-report)
