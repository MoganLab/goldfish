;; pi-make-compaction-option
;; 构造自动压缩选项：启用时返回含 compactor 的配置项，未启用时返回 #f。
;;
;; 语法
;; ----
;; (pi-make-compaction-option parsed cfg sfile state timeout . opt-emit)
;;
;; 参数
;; ----
;; parsed : alist
;; 解析后的命令行参数 alist；--no-auto-compact 时禁用自动压缩。
;; cfg : alist
;; 运行配置 alist，含 config-dir（配置文件目录）。
;; sfile : string | #f
;; 会话文件路径；#f 表示无会话文件，不启用自动压缩。
;; state : vector
;; 会话状态向量（元素 0 为消息列表，元素 1 为消息条数）。
;; timeout : number
;; 单轮请求超时时间（秒）。
;; opt-emit : procedure
;; 可选事件回调；压缩完成后以 'compaction 事件通知前端。
;;
;; 返回值
;; ----
;; list | #f
;; 启用时返回 ((compactor . procedure))，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii path)
  (liii pi code core compaction flow)
) ;import

(check-set-mode! 'report-failed)

(define CONFIG-DIR (path-join (path-temp-dir) "pi-make-compaction-option-test"))

(define STATE (vector '() 0))

(define (write-settings enabled?)
  (path-mkdir CONFIG-DIR :parents #t :exist-ok #t)
  (path-write-text (path-join CONFIG-DIR (path "settings.json"))
    (string-append "{\"compaction\":{\"enabled\":"
      (if enabled? "true" "false")
      "}}"
    ) ;string-append
  ) ;path-write-text
) ;define

(define CFG (list (cons 'config-dir CONFIG-DIR)))

;; 1. 未启用自动压缩：返回 #f
(write-settings #f)
(check-false (pi-make-compaction-option '() CFG "session.jsonl" STATE 600))

;; 2. 启用自动压缩：返回含 compactor 的配置项
(write-settings #t)
(let ((opt (pi-make-compaction-option '() CFG "session.jsonl" STATE 600)))
  (check-true (pair? opt))
  (check (car (car opt)) => 'compactor)
  (check-true (procedure? (cdr (car opt))))
) ;let

;; 3. 无会话文件时不启用
(check-false (pi-make-compaction-option '() CFG #f STATE 600))

;; 4. --no-auto-compact 时不启用
(check-false (pi-make-compaction-option (list (cons 'no-auto-compact #t))
               CFG
               "session.jsonl"
               STATE
               600
             ) ;pi-make-compaction-option
) ;check-false

(check-report)
