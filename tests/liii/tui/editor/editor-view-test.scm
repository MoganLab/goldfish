(import (liii check) (liii tui editor view) (liii tui text-width))

(check-set-mode! 'report-failed)

;; render-editor-lines & render-line-with-cursor 渲染视图测试

;; 1. render-line-with-cursor 未聚焦时原样返回
(check (render-line-with-cursor "hello" 2 #f 'inverse) => "hello")

;; 2. render-line-with-cursor 聚焦反显光标 (行中)
(check (render-line-with-cursor "hello" 1 #t 'inverse)
  =>
  "h\x1b;[7me\x1b;[0mllo"
) ;check

;; 3. render-line-with-cursor 聚焦反显光标 (行尾追加空格反显)
(check (render-line-with-cursor "hello" 5 #t 'inverse)
  =>
  "hello\x1b;[7m \x1b;[0m"
) ;check

;; 4. render-line-with-cursor 竖线光标样式
(check (render-line-with-cursor "hello" 2 #t 'bar) => "he|llo")

;; 5. 空内容且未聚焦显示 placeholder
(check (render-editor-lines '("") 0 0 #f "Type here..." 40 'inverse)
  =>
  '("Type here...")
) ;check

;; 6. 空内容但聚焦时显示光标
(check (render-editor-lines '("") 0 0 #t "Type here..." 40 'inverse)
  =>
  '("\x1b;[7m \x1b;[0m")
) ;check

;; 7. 多行渲染，仅光标所在行绘制光标
(let ((lines '("line 1" "line 2")))
  (check (render-editor-lines lines 1 0 #t "" 40 'inverse)
    =>
    '("line 1" "\x1b;[7ml\x1b;[0mine 2")
  ) ;check
) ;let

;; 8. 包含中文字符的反显与竖线光标样式
(check (render-line-with-cursor "你好世界" 1 #t 'inverse)
  =>
  "你\x1b;[7m好\x1b;[0m世界"
) ;check
(check (render-line-with-cursor "你好世界" 1 #t 'bar) => "你|好世界")
(check (render-line-with-cursor "你好" 2 #t 'inverse)
  =>
  "你好\x1b;[7m \x1b;[0m"
) ;check

;; 9. 硬件光标样式原样返回行内容（不切分字符串）
(check (render-line-with-cursor "hello" 2 #t 'hardware) => "hello")
(check (render-line-with-cursor "你好世界" 2 #t 'hardware)
  =>
  "你好世界"
) ;check

;; 10. 超宽内容行按显示宽度折行，而非依赖终端硬换行
(check (render-editor-lines '("hello world") 0 0 #f "" 6 'hardware)
  =>
  '("hello" "world")
) ;check

;; 逗号分词的数字串在 token 边界换行，不拆开数字
(check (render-editor-lines '("12,34,56") 0 0 #f "" 5 'hardware)
  =>
  '("12,34" ",56")
) ;check

;; 11. 折行后的视觉光标坐标按完整逻辑行和行内前缀计算
(check (editor-visual-cursor '("hello world" "next") 1 2 6) => '(2 2))
(check (editor-visual-cursor '("你好世界") 0 3 4) => '(1 2))

(check-report)
