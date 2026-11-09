;; %repl-ctrl-c-action
;; REPL 中按 Ctrl+C 时的动作决策函数。
;; 永不返回表示退出的值，确保 Ctrl+C 仅具备清空/中断语义。
;;
;; 语法
;; ----
;; (%repl-ctrl-c-action buf-pending? input-empty?)
;;
;; 参数
;; ----
;; buf-pending? : boolean
;; 多行缓冲是否非空。
;; input-empty? : boolean
;; 当前单行输入框是否为空。
;;
;; 返回值
;; ----
;; symbol
;; 'clear-buffer、'clear-input 或 'noop。

(import (scheme base) (liii check) (liii goldrepl loop))

(check-set-mode! 'report-failed)

;; 1. 多行缓冲非空时：无论当前行是否为空，一律清空多行缓冲
(check (%repl-ctrl-c-action #t #t) => 'clear-buffer)
(check (%repl-ctrl-c-action #t #f) => 'clear-buffer)

;; 2. 多行缓冲为空且输入框非空时：清空当前输入行
(check (%repl-ctrl-c-action #f #f) => 'clear-input)

;; 3. 多行缓冲为空且输入框为空时：返回 'noop，永不返回 'quit 或退出相关标志
(check (%repl-ctrl-c-action #f #t) => 'noop)
(check-true (not (eq? (%repl-ctrl-c-action #f #t) 'quit)))

(check-report)
