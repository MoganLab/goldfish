(import (liii check)
        (liii tui text-width)
        (liii tui box border))

(check-set-mode! 'report-failed)

;; border-style & border rendering
;; 边框样式定义、解析与边框文本渲染纯函数

;; 1. 内置边框风格解析
(let ((b-single (border-style-resolve 'single)))
  (check (border-style? b-single) => #t)
  (check (border-style-name b-single) => 'single)
  (check (border-style-tl b-single) => "┌")
  (check (border-style-tr b-single) => "┐")
  (check (border-style-bl b-single) => "└")
  (check (border-style-br b-single) => "┘")
  (check (border-style-h b-single) => "─")
  (check (border-style-v b-single) => "│"))

(let ((b-double (border-style-resolve 'double)))
  (check (border-style-name b-double) => 'double)
  (check (border-style-tl b-double) => "╔")
  (check (border-style-tr b-double) => "╗")
  (check (border-style-bl b-double) => "╚")
  (check (border-style-br b-double) => "╝")
  (check (border-style-h b-double) => "═")
  (check (border-style-v b-double) => "║"))

(let ((b-rounded (border-style-resolve 'rounded)))
  (check (border-style-name b-rounded) => 'rounded)
  (check (border-style-tl b-rounded) => "╭")
  (check (border-style-tr b-rounded) => "╮")
  (check (border-style-bl b-rounded) => "╰")
  (check (border-style-br b-rounded) => "╯")
  (check (border-style-h b-rounded) => "─")
  (check (border-style-v b-rounded) => "│"))

(let ((b-ascii (border-style-resolve 'ascii)))
  (check (border-style-name b-ascii) => 'ascii)
  (check (border-style-tl b-ascii) => "+")
  (check (border-style-tr b-ascii) => "+")
  (check (border-style-bl b-ascii) => "+")
  (check (border-style-br b-ascii) => "+")
  (check (border-style-h b-ascii) => "-")
  (check (border-style-v b-ascii) => "|"))

(let ((b-none (border-style-resolve 'none)))
  (check (border-style-name b-none) => 'none)
  (check (border-style-tl b-none) => "")
  (check (border-style-tr b-none) => "")
  (check (border-style-bl b-none) => "")
  (check (border-style-br b-none) => "")
  (check (border-style-h b-none) => "")
  (check (border-style-v b-none) => ""))

;; 2. 自定义边框风格构造
(let ((custom (make-border-style "1" "2" "3" "4" "=" "!")))
  (check (border-style? custom) => #t)
  (check (border-style-name custom) => 'custom)
  (check (border-style-tl custom) => "1")
  (check (border-style-tr custom) => "2")
  (check (border-style-bl custom) => "3")
  (check (border-style-br custom) => "4")
  (check (border-style-h custom) => "=")
  (check (border-style-v custom) => "!")
  (check (border-style-resolve custom) => custom))

;; 非法风格解析拦截
(check-catch 'value-error (border-style-resolve 'unknown))
(check-catch 'type-error (border-style-resolve 123))

;; 3. 边框顶行渲染 (border-render-top)
(let ((single (border-style-resolve 'single))
      (ascii (border-style-resolve 'ascii))
      (none (border-style-resolve 'none)))
  ;; ASCII 顶行
  (check (border-render-top ascii 10) => "+--------+")
  ;; Unicode single 顶行
  (let ((top10 (border-render-top single 10)))
    (check (display-width top10) => 10)
    (check top10 => "┌────────┐"))
  ;; 'none 风格顶行返回空字符串
  (check (border-render-top none 10) => "")

  ;; 嵌入标题
  (let ((top-title (border-render-top single 16 "Menu")))
    (check (display-width top-title) => 16)
    (check top-title => "┌─ Menu ───────┐"))

  ;; 嵌入 CJK 标题
  (let ((top-cjk (border-render-top single 16 "菜单")))
    (check (display-width top-cjk) => 16)
    (check top-cjk => "┌─ 菜单 ───────┐"))

  ;; 标题过长时安全截断
  (let ((top-long (border-render-top single 14 "SuperLongTitleHere")))
    (check (display-width top-long) => 14))

  ;; 宽度过窄时的边界安全处理
  (check (border-render-top single 0) => "")
  (check (border-render-top single 1) => "┌")
  (check (border-render-top single 2) => "┌┐"))

;; 4. 边框底行渲染 (border-render-bottom)
(let ((single (border-style-resolve 'single))
      (ascii (border-style-resolve 'ascii))
      (none (border-style-resolve 'none)))
  (check (border-render-bottom ascii 10) => "+--------+")
  (let ((bot10 (border-render-bottom single 10)))
    (check (display-width bot10) => 10)
    (check bot10 => "└────────┘"))
  (check (border-render-bottom none 10) => "")
  (check (border-render-bottom single 0) => "")
  (check (border-render-bottom single 1) => "└")
  (check (border-render-bottom single 2) => "└┘"))

;; 5. 边框内容行渲染 (border-render-line)
(let ((single (border-style-resolve 'single))
      (none (border-style-resolve 'none)))
  ;; 基础内容行：border-v + pad-left + content + pad-right + border-v
  ;; content-w=6, pad-left=1, pad-right=1 -> 总宽度 1 + 1 + 6 + 1 + 1 = 10
  (let ((line (border-render-line single "hi" 6 1 1)))
    (check (display-width line) => 10)
    (check line => "│ hi     │"))

  ;; 中文字符内容行
  (let ((line-cjk (border-render-line single "你好" 6 1 1)))
    (check (display-width line-cjk) => 10)
    (check line-cjk => "│ 你好   │"))

  ;; 'none 风格无两侧边框
  (let ((line-none (border-render-line none "hi" 6 1 1)))
    (check (display-width line-none) => 8)
    (check line-none => " hi     ")))

(check-report)
