(import (liii check)
        (liii tui component))

;; make-text
;; 创建自动折行换行的文本组件。
;;
;; 语法
;; ----
;; (make-text str)
;;
;; 参数
;; ----
;; str : string
;; 要显示的文本内容。
;;
;; 返回值
;; ----
;; component
;; 支持自动折行的文本组件实例。
;;
;; 说明
;; ----
;; 1. 支持自动折行：当单行长度超出 width 时，自动按单词或边界折行；
;; 2. 支持换行符：内嵌 \n 优先切分为多行，每行独立计算折行；
;; 3. 边界容错：若 width <= 0 返回空行列表 '("")，防止死循环；空字符串渲染为 '("")；
;; 4. 若参数 str 不是字符串，抛出 type-error。

;; 普通单行未超出宽度
(let ((c (make-text "hello world")))
  (check (component-render c 20) => '("hello world")))

;; 自动折行（空格优先）
(let ((c (make-text "hello world")))
  (check (component-render c 7) => '("hello" "world"))
  (check (component-render c 5) => '("hello" "world")))

;; 长单词超出宽度（硬截断）
(let ((c (make-text "abcdefghij")))
  (check (component-render c 4) => '("abcd" "efgh" "ij")))

;; 内嵌换行符
(let ((c (make-text "line1\nline2\nline3")))
  (check (component-render c 20) => '("line1" "line2" "line3")))

;; 内嵌换行与折行混合
(let ((c (make-text "hello world\nshort\nlongwordisverylong indeed")))
  (check (component-render c 8) => '("hello" "world" "short" "longword" "isverylo" "ng" "indeed")))

;; 空字符串
(let ((c (make-text "")))
  (check (component-render c 20) => '("")))

;; 宽度 <= 0 防御
(let ((c (make-text "hello")))
  (check (component-render c 0) => '(""))
  (check (component-render c -5) => '("")))

;; CJK 表意字符与全角标点折行
(let ((c (make-text "金鱼Scheme终端用户界面")))
  (check (component-render c 10) => '("金鱼Scheme" "终端用户界" "面")))

;; ANSI 转义序列安全折行
(let ((c (make-text "\x1b[32mhello world\x1b[0m")))
  (check (component-render c 5) => '("\x1b[32mhello\x1b[0m" "\x1b[32mworld\x1b[0m")))

;; 参数非法检查
(check-catch 'type-error (make-text 123))
(check-catch 'type-error (make-text 'symbol))

(check-report)
