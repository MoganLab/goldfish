;; gfproject-tool-conflict?
;; 检测工具配置中是否存在本地与内置冲突。
;;
;; 语法
;; ----
;; (gfproject-tool-conflict? command bundle) -> boolean
;;
;; 参数
;; ----
;; command : string
;; 命令名称。
;;
;; bundle : alist
;; gfproject-resolve-tool-bundle 解析出的包字典。
;;
;; 返回值
;; ----
;; boolean
;; 存在本地与内置同名冲突时返回 #t，否则返回 #f。

(import (liii check)
  (liii gfproject tools)
) ;import

(check-set-mode! 'report-failed)

;; 1. 同源路径不冲突
(check
  (gfproject-tool-conflict? "doc"
    '(("lib_path" . "/repo/goldfish/../gfproject.json")
      ("local_path" . "/repo/gfproject.json")
      ("lib_tools" ("doc" ("module" . "golddoc")))
      ("local_tools" ("doc" ("module" . "golddoc"))))
  ) ;gfproject-tool-conflict?
  => #f
) ;check

;; 2. 真实已注册内置工具不冲突
(let ((bundle (gfproject-resolve-tool-bundle "doc")))
  (check (gfproject-tool-conflict? "doc" bundle) => #f)
) ;let

(check-report)
