;; pi-get-agent-dir
;; 解析 agent 目录（会话与扩展的默认位置），返回绝对路径。
;;
;; 语法
;; ----
;; (pi-get-agent-dir [dir]) -> path
;;
;; 参数
;; ----
;; dir : string
;; 可选显式目录；缺省时读取 PI_CODING_AGENT_DIR 或 ~/.pi/agent。
;;
;; 返回值
;; ----
;; path
;; 绝对化后的 agent 目录 path 对象。

(import (liii check)
  (liii path)
  (liii string)
  (liii pi code config)
) ;import

(check-set-mode! 'report-failed)

(check (path-absolute? (pi-get-agent-dir "/tmp/gf-agent-dir-test")) => #t)
(check (string-ends? (path->string (pi-get-agent-dir "/tmp/gf-agent-dir-test"))
         "gf-agent-dir-test"
       ) ;string-ends?
  =>
  #t
) ;check
(check (path-absolute? (pi-get-agent-dir)) => #t)

(check-report)
