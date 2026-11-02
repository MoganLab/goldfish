;; pi-worker-ensure-session-file!
;; 确保会话文件已被惰性创建并缓存于 box 中。
;;
;; 语法
;; ----
;; (pi-worker-ensure-session-file! parsed cfg sfile-box)
;;
;; 参数
;; ----
;; parsed : list?
;; 命令行参数解析结果。
;; cfg : list?
;; 配置列表。
;; sfile-box : vector?
;; 长度为 1 的向量容器，用于缓存会话文件路径。
;;
;; 返回值
;; ----
;; any
;; 会话文件路径或 #f。

(import (scheme base)
  (liii check)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; 1. sfile-box 已有值时直接返回既有会话文件
(let ((box (vector "existing-session.jsonl")))
  (check (pi-worker-ensure-session-file! '() '() box) => "existing-session.jsonl")
  (check (vector-ref box 0) => "existing-session.jsonl")
) ;let

(check-report)
