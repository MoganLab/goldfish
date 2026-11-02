(import (liii check)
        (liii tui terminal))

(check-set-mode! 'report-failed)

;; make-standard-terminal
;; 创建基于 ANSI 转义序列的标准/真实终端抽象实例。
;;
;; 语法
;; ----
;; (make-standard-terminal [:output-port port] [:detect-size-fn fn])
;;
;; 参数
;; ----
;; port : output-port, 可选，输出目标端口，默认为 (current-output-port)。
;; fn : procedure, 可选，用于动态获取尺寸的过程，默认为 terminal-detect-size。
;;
;; 返回值
;; ----
;; terminal
;; 标准终端实例。

(let* ((p (open-output-string))
       (st (make-standard-terminal :output-port p
                                   :detect-size-fn (lambda () '(100 30)))))
  (check (terminal? st) => #t)
  (check (standard-terminal? st) => #t)
  (check (virtual-terminal? st) => #f)

  ;; 尺寸查询
  (check (terminal-size st) => '(100 30))

  ;; 写入文本
  (terminal-write st "hello")
  (check (get-output-string p) => "hello")

  ;; 设置光标（ANSI 1-indexed: row 3, col 6 -> CR + ESC [ 3 ; 6 H，
  ;; CR 前置保护满宽行末列的待定单元格）
  (terminal-set-cursor! st 5 2)
  (check (terminal-cursor st) => '(5 2))
  (check (get-output-string p) => "hello\r\x1b[3;6H")

  ;; 清除当前行
  (terminal-clear-line st)
  (check (get-output-string p) => "hello\r\x1b[3;6H\x1b[2K\r")

  ;; 整体清屏与光标复位
  (terminal-clear st)
  (check (terminal-cursor st) => '(0 0))
  (check (get-output-string p) => "hello\r\x1b[3;6H\x1b[2K\r\x1b[2J\x1b[H")

  ;; 光标显隐控制
  (terminal-hide-cursor! st)
  (check (terminal-cursor-visible? st) => #f)
  (check (get-output-string p) => "hello\r\x1b[3;6H\x1b[2K\r\x1b[2J\x1b[H\x1b[?25l")

  (terminal-show-cursor! st)
  (check (terminal-cursor-visible? st) => #t)
  (check (get-output-string p) => "hello\r\x1b[3;6H\x1b[2K\r\x1b[2J\x1b[H\x1b[?25l\x1b[?25h")

  ;; 刷新输出
  (check (terminal-flush st) => #t))

;; 类型校验
(check-catch 'type-error (make-standard-terminal :output-port "not-a-port"))
(check-catch 'type-error (make-standard-terminal :detect-size-fn "not-a-proc"))

(check-report)
