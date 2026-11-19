;; standard-terminal-inline
;; 标准终端 inline 模式下的相对光标移动行为。
;;
;; inline 模式下内容行数可以超过终端高度，由终端自身滚动，
;; 因此光标移动采用相对定位（CR + ESC[nA / ESC[nC）而非绝对定位。
;;
;; 语法
;; ----
;; (make-standard-terminal :output-port port :inline #t)
;; (terminal-set-cursor! term x y)
;;
;; 参数
;; ----
;; port : output-port
;; 标准终端的输出目标端口。
;;
;; term : terminal?
;; 标准终端实例。
;;
;; x : integer?
;; 目标列坐标（0-based）。
;;
;; y : integer?
;; 目标行坐标（0-based）。
;;
;; 返回值
;; ----
;; unspecified
;; 光标定位过程未指定具体返回值。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report)

;; 1. 测试 inline 模式下的相对光标移动
(let* ((p (open-output-string))
       (term (make-standard-terminal :output-port p :inline #t))
      ) ;
  (check-true (standard-terminal? term))
  (check (terminal-cursor term) => '(0 0))

  ;; 从 (0, 0) 移动到未渲染的 (0, 2)：开辟新行输出 \r\n\r\n
  (terminal-set-cursor! term 0 2)
  (check (get-output-string p) => "\r\n\r\n")
  (check (terminal-cursor term) => '(0 2))

  ;; 从 (0, 2) 向上移动到 (4, 1)：CR 先清除待换行状态，再相对上移 1 行、右移 4 列
  (terminal-set-cursor! term 4 1)
  (check (terminal-cursor term) => '(4 1))
  (check (get-output-string p) => "\r\n\r\n\r\x1b;[1A\x1b;[4C")
) ;let*

;; 2. 非 inline 模式为 CR 前置的绝对定位：先清除待换行状态，
;;    保护满宽行末列的待定单元格（Windows conpty 否则丢弃末列字符）
(let* ((p (open-output-string)) (term (make-standard-terminal :output-port p)))
  (terminal-set-cursor! term 4 1)
  (check (get-output-string p) => "\r\x1b;[2;5H")
  (check (terminal-cursor term) => '(4 1))
) ;let*

;; 3. 已渲染行内的下移也是 CR 前置：写满末列后的待换行状态
;;    必须先被 \r 清除，再做 ESC[nB 相对下移（Windows 吞字回归）
(let* ((p (open-output-string))
       (term (make-standard-terminal :output-port p :inline #t))
      ) ;
  (terminal-set-cursor! term 0 2)
  (terminal-set-cursor! term 0 1)
  (check (get-output-string p) => "\r\n\r\n\r\x1b;[1A")
) ;let*

(check-report)
