;; make-terminal-record
;; 创建一个底层终端协议记录实例。
;;
;; 语法
;; ----
;; (make-terminal-record type get-size-proc write-proc get-cursor-proc
;;   set-cursor-proc clear-proc clear-line-proc clear-line-to-end-proc
;;   begin-sync-proc end-sync-proc get-cursor-visible-proc
;;   set-cursor-visible-proc flush-proc max-y-proc data)
;;
;; 参数
;; ----
;; type : symbol?
;; 终端类型标识，如 'virtual、'standard。
;;
;; get-size-proc : procedure
;; 返回终端尺寸的 (width . height) 的过程。
;;
;; write-proc : procedure
;; 向终端写入文本的过程。
;;
;; get-cursor-proc : procedure
;; 返回当前光标位置的 (x . y) 的过程。
;;
;; set-cursor-proc : procedure
;; 设置光标位置的过程。
;;
;; clear-proc : procedure
;; 清空整个终端表面的过程。
;;
;; clear-line-proc : procedure
;; 清空当前行的过程。
;;
;; clear-line-to-end-proc : procedure
;; 清除光标到行尾的过程。
;;
;; begin-sync-proc : procedure
;; 开始终端同步输出的过程。
;;
;; end-sync-proc : procedure
;; 结束终端同步输出的过程。
;;
;; get-cursor-visible-proc : procedure
;; 查询光标是否可见的过程。
;;
;; set-cursor-visible-proc : procedure
;; 设置光标可见性的过程。
;;
;; flush-proc : procedure
;; 刷新输出缓冲的过程。
;;
;; max-y-proc : procedure
;; 返回终端曾渲染到达的最大行索引的过程。
;;
;; data : any
;; 终端实例附带的私有数据。
;;
;; 返回值
;; ----
;; terminal?
;; 终端协议记录实例。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui terminal protocol)
) ;import

(check-set-mode! 'report-failed)

(define (%noop)
  #t
) ;define

(let ((term (make-terminal-record 'custom
              (lambda (t) '(80 24))
              (lambda (t text) #t)
              (lambda (t) '(0 0))
              (lambda (t x y) #t)
              (lambda (t) #t)
              (lambda (t) #t)
              (lambda (t) #t)
              (lambda (t) #t)
              (lambda (t) #t)
              (lambda (t) #t)
              (lambda (t v) #t)
              (lambda (t) #t)
              (lambda (t) 0)
              'payload
            ) ;make-terminal-record
      ) ;term
     ) ;
  (check-true (terminal? term))
  (check (terminal-type term) => 'custom)
  (check (terminal-size term) => '(80 24))
  (check (terminal-cursor term) => '(0 0))
  (check (terminal-data term) => 'payload)
  (check (terminal-flush term) => #t)
  (check (terminal-max-y term) => 0)
) ;let

(check-report)
