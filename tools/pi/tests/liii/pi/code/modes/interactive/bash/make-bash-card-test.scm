;; make-bash-card
;; 构造并返回新的 make-bash-card 实例组件。
;;
;; 语法
;; ----
;; (make-bash-card ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; component?
;; 返回构造的实例。

(import (scheme base)
  (liii check)
  (liii string)
  (liii unicode)
  (liii tui)
  (liii pi code modes interactive bash))

(check-set-mode! 'report)

;; 运行中卡片：绿色上下边框 + "$ cmd" 头 + Running... 提示
(let* ((card (make-bash-card "ls"))
       (lines (component-render card 80))
       (plain (map strip-ansi lines)))
  ;; 边框 + 头 + Running + 边框 + 尾部空行 = 5 行
  (check (length lines) => 5)
  (check (car plain) => (utf8-make-string 80 #\─))
  (check-true (string-contains? (car lines) "38;2;94;178;134"))
  (check (list-ref plain 1) => " $ ls")
  (check-true (string-contains? (list-ref plain 2) "Running"))
  (check-true (string-contains? (list-ref lines 1) "38;2;94;178;134"))
  (check (list-ref plain 3) => (utf8-make-string 80 #\─))
  (check (list-ref plain 4) => ""))

(check-report)
