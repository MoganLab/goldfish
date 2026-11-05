;; run-compact-flow
;; 手动压缩流程：压缩最近一次会话，并（可选）继续本轮请求。
;;
;; 语法
;; ----
;; (run-compact-flow parsed cfg initial-msg emit)
;;
;; 参数
;; ----
;; parsed : alist
;; 解析后的命令行参数 alist；session-dir 指定会话目录。
;; cfg : alist
;; 运行配置 alist，含 config-dir 等配置项。
;; initial-msg : string
;; 本轮用户消息文本；为空时只压缩不继续请求。
;; emit : procedure | #f
;; 事件派发回调。
;;
;; 返回值
;; ----
;; integer | unspecified
;; 只压缩不继续请求时返回 0；继续请求时返回请求结果。

(import (scheme base)
  (liii check)
  (liii path)
  (liii pi code modes flow)
) ;import

(check-set-mode! 'report-failed)

(define CONFIG-DIR (path-join (path-temp-dir) "pi-run-compact-flow-test"))

(define SESSION-DIR (path-join (path-temp-dir) "pi-run-compact-flow-sessions"))

(path-mkdir CONFIG-DIR :parents #t :exist-ok #t)
(path-mkdir SESSION-DIR :parents #t :exist-ok #t)

(define CFG
  (list (cons 'tools '())
    (cons 'provider "google")
    (cons 'model-id "gemini-3.8-flash")
    (cons 'config-dir CONFIG-DIR)
  ) ;list
) ;define

(define PARSED (list (cons 'session-dir (path->string SESSION-DIR))))

;; 1. 会话目录内没有会话文件时报错（无可压缩目标）
(check-catch 'compact-error (run-compact-flow PARSED CFG "" #f))

;; 2. 会话目录内没有会话文件时，带 prompt 同样报错
(check-catch 'compact-error (run-compact-flow PARSED CFG "follow up" #f))

(check-report)
