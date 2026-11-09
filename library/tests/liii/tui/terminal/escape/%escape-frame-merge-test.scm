;; %escape-frame-merge
;; 把 ESC CR 之后尾随的 LF 并入同一转义帧。
;;
;; 语法
;; ----
;; (%escape-frame-merge bytes next-byte)
;;
;; 参数
;; ----
;; bytes : list?
;; 已判定完整的转义帧（按到达顺序排列）。
;; next-byte : procedure
;; 取下一个字节的零参过程；无可用字节时返回 #f。
;;
;; 返回值
;; ----
;; 两个值：(合并后的字节列表 需要回填的字节或 #f)
;;
;; 说明
;; ----
;; 部分终端把 Alt+Enter 发成 ESC CR LF 三字节。若在 ESC CR 处就判定帧完整，
;; 裸 LF 会滞留在输入队列，下一轮被解析成 ctrl-j，在已清空的输入框里打出
;; 幽灵换行。这里在 10ms 窗口内尝试并入 LF：
;; - 取不到字节：原帧返回，不回填；
;; - 取到 LF：把它并入原帧；
;; - 取到其他字节：原帧返回，并把该字节交回调用方回填（不丢键）。
;; 非 ESC CR 帧原样返回，不读取任何字节。

(import (scheme base) (liii check) (liii tui terminal escape))

(check-set-mode! 'report-failed)

;; ESC CR 后紧跟 LF：三字节合并为一帧
(check (call-with-values
         (lambda () (%escape-frame-merge (list 27 13) (lambda () 10)))
         list
       ) ;call-with-values
  =>
  (list (list 27 13 10) #f)
) ;check

;; ESC CR 后没有更多字节：保持两字节帧
(check (call-with-values
         (lambda () (%escape-frame-merge (list 27 13) (lambda () #f)))
         list
       ) ;call-with-values
  =>
  (list (list 27 13) #f)
) ;check

;; ESC CR 后到达其他字节：原帧返回，并把该字节交回回填
(check (call-with-values
         (lambda () (%escape-frame-merge (list 27 13) (lambda () 97)))
         list
       ) ;call-with-values
  =>
  (list (list 27 13) 97)
) ;check

;; 非 ESC CR 帧不读取、不改写，取字节过程不得被调用
(check (call-with-values
         (lambda () (%escape-frame-merge (list 27 10) (lambda () (error "must not read"))))
         list
       ) ;call-with-values
  =>
  (list (list 27 10) #f)
) ;check

;; CSI 帧与 SGR 鼠标帧同样原样返回
(check (call-with-values
         (lambda () (%escape-frame-merge (list 27 91 65) (lambda () (error "must not read"))))
         list
       ) ;call-with-values
  =>
  (list (list 27 91 65) #f)
) ;check

(check-report)
