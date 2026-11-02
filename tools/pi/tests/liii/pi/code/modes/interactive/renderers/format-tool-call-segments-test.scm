;; format-tool-call-segments
;; format-tool-call-segments 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (format-tool-call-segments ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base) (liii check) (liii pi code modes interactive renderers))

(check-set-mode! 'report-failed)

;; 各内置工具的着色分段（对齐 pi renderCall 渲染器）
;; 角色: bold=toolTitle 粗体 / accent=路径或模式 / muted=次要信息 / warning=范围
(check (format-tool-call-segments "bash" '((command . "npm test")
                                           (timeout . 30)))
  =>
  '((bold . "$ npm test") (muted . " (timeout 30s)"))
) ;check
(check (format-tool-call-segments "bash" '(("command" . "ls -la")))
  =>
  '((bold . "$ ls -la"))
) ;check

(check (format-tool-call-segments "read"
         '(("file_path" . "src/app.scm") ("offset" . 10) ("limit" . 20))
       ) ;format-tool-call-segments
  =>
  '((bold . "read") (accent . " src/app.scm") (warning . ":10-29"))
) ;check

(check (format-tool-call-segments "write" '(("path" . "README.md")))
  =>
  '((bold . "write") (accent . " README.md"))
) ;check
(check (format-tool-call-segments "edit" "main.scm")
  =>
  '((bold . "edit") (accent . " main.scm"))
) ;check

(check (format-tool-call-segments "grep" '(("pattern" . "define")
                                           ("path" . "src/")))
  =>
  '((bold . "grep") (accent . " /define/") (muted . " in src/"))
) ;check

(check (format-tool-call-segments "find" '(("pattern" . "*.scm")))
  =>
  '((bold . "find") (accent . " *.scm") (muted . " in ."))
) ;check

(check (format-tool-call-segments "ls" '(("path" . "src/")))
  =>
  '((bold . "ls") (accent . " src/"))
) ;check

(check (format-tool-call-segments "web_search" '(("query" . "Goldfish Scheme")))
  =>
  '((bold . "web_search") (accent . " \"Goldfish Scheme\""))
) ;check

(check (format-tool-call-segments "custom_tool" '(("key" . "value")))
  =>
  '((bold . "custom_tool") (muted . " key=value"))
) ;check

(check-report)
