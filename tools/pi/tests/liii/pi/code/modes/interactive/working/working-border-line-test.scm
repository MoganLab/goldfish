;; working-border-line
;; 渲染包含当前动画帧与 Working 文本的边框分割线。
;;
;; 语法
;; ----
;; (working-border-line ind width color)
;;
;; 参数
;; ----
;; ind : working-indicator?
;; 工作指示器实例。
;; width : integer?
;; 边框行目标总显示宽度。
;; color : string?
;; ANSI 颜色控制码字符串。
;;
;; 返回值
;; ----
;; string?
;; 格式化后的彩色边框行字符串。

(import (scheme base)
  (liii check)
  (liii string)
  (liii unicode)
  (liii tui)
  (liii pi code modes interactive working)
) ;import

(check-set-mode! 'report)

;; 渲染格式：── ⠋ Working ──… 填满整行，颜色由调用方注入
(let ((ind (make-working-indicator)))
  (let* ((line (working-border-line ind 100 "\x1b;[38;2;151;118;229m"))
         (plain (strip-ansi line))
         (dashes (utf8-make-string (- 100 13) #\─))
        ) ;
    ;; 纯文本格式：── + 空格 + spinner + 空格 + Working + 空格 + 剩余横线
    (check plain => (string-append "── ⠋ Working " dashes))
    ;; 可见宽度恰为终端宽度
    (check (display-width plain) => 100)
    ;; 使用调用方注入的主题色（spinner/文字/边框线同色）
    (check-true (string-contains? line "\x1b;[38;2;151;118;229m"))
    ;; 复位色尾码
    (check-true (string-contains? line "\x1b;[0m"))
  ) ;let*
  ;; 帧推进后行内容随之变化
  (working-advance! ind)
  (check-true (string-contains? (strip-ansi (working-border-line ind 100 "\x1b;[38;2;151;118;229m"))
                "⠙"
              ) ;string-contains?
  ) ;check-true
) ;let

(check-report)
