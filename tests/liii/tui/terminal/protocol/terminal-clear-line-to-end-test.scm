;; terminal-clear-line-to-end
;; 清除终端当前行从光标位置到行尾的内容。
;;
;; 语法
;; ----
;; (terminal-clear-line-to-end term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii string) (liii tui terminal))

(check-set-mode! 'report-failed)

;; 1. virtual-terminal: 清除光标后字符并保留光标前字符
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (terminal-write vt "0123456789")
  (check (car (virtual-terminal-screen vt)) => "0123456789")
  ;; 移动光标到索引 5
  (terminal-set-cursor! vt 5 0)
  (check (terminal-cursor vt) => '(5 0))
  ;; 清除光标后到行尾
  (terminal-clear-line-to-end vt)
  ;; 光标前 0..4 保持，5..9 变为空格
  (check (car (virtual-terminal-screen vt)) => "01234     ")
  ;; 验证 ANSI 序列写入了 virtual-terminal-raw
  (check (pair? (member "\x1b;[K" (virtual-terminal-raw vt))) => #t)
) ;let

;; 2. virtual-terminal: 在行首清除整行
(let ((vt (make-virtual-terminal :width 8 :height 2)))
  (terminal-write vt "abcdefgh")
  (terminal-set-cursor! vt 0 0)
  (terminal-clear-line-to-end vt)
  (check (car (virtual-terminal-screen vt)) => "        ")
) ;let

;; 3. virtual-terminal: 在行尾清除无影响
(let ((vt (make-virtual-terminal :width 5 :height 2)))
  (terminal-write vt "abc")
  (terminal-set-cursor! vt 3 0)
  (terminal-clear-line-to-end vt)
  (check (car (virtual-terminal-screen vt)) => "abc  ")
) ;let

;; 4. standard-terminal: 写入 \x1b;[K 序列
(let* ((p (open-output-string)) (st (make-standard-terminal :output-port p)))
  (terminal-clear-line-to-end st)
  (check (string-contains? (get-output-string p) "\x1b;[K") => #t)
) ;let*

;; 5. 类型检查
(check-catch 'type-error (terminal-clear-line-to-end "not-a-terminal"))
(check-catch 'type-error (terminal-clear-line-to-end 12345))

(check-report)
