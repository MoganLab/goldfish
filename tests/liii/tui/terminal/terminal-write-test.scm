(import (liii check)
        (liii tui terminal))

(check-set-mode! 'report-failed)

;; terminal-write
;; 向终端当前光标位置写入文本。
;;
;; 语法
;; ----
;; (terminal-write term text)
;;
;; 参数
;; ----
;; term : terminal
;; 终端实例。
;; text : string
;; 待写入的文本。
;;
;; 返回值
;; ----
;; unspecified

;; 写入普通文本
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (terminal-write vt "hello")
  (check (terminal-cursor vt) => '(5 0))
  (let ((screen (virtual-terminal-screen vt)))
    (check (car screen) => "hello     ")
    (check (cadr screen) => "          ")
    (check (caddr screen) => "          "))
  ;; 追加写入
  (terminal-write vt "123")
  (check (terminal-cursor vt) => '(8 0))
  (check (car (virtual-terminal-screen vt)) => "hello123  "))

;; 写入换行符 \n
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (terminal-write vt "foo\nbar")
  (check (terminal-cursor vt) => '(3 1))
  (let ((screen (virtual-terminal-screen vt)))
    (check (car screen) => "foo       ")
    (check (cadr screen) => "bar       ")
    (check (caddr screen) => "          ")))

;; 写入回车符 \r 覆盖
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (terminal-write vt "hello\rabc")
  (check (terminal-cursor vt) => '(3 0))
  (check (car (virtual-terminal-screen vt)) => "abclo     "))

;; 超出屏幕高度自动向上滚动
(let ((vt (make-virtual-terminal :width 8 :height 2)))
  (terminal-write vt "line1\nline2\nline3")
  (check (terminal-cursor vt) => '(5 1))
  (let ((screen (virtual-terminal-screen vt)))
    (check (car screen) => "line2   ")
    (check (cadr screen) => "line3   ")))

;; 原始日志记录与 flush
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (terminal-write vt "one")
  (terminal-write vt "two")
  (check (virtual-terminal-raw vt) => '("one" "two"))
  (check (terminal-flush vt) => #t))

;; 类型检查
(check-catch 'type-error (terminal-write "not-term" "text"))
(check-catch 'type-error (terminal-write (make-virtual-terminal :width 10 :height 3) 123))

(check-report)
