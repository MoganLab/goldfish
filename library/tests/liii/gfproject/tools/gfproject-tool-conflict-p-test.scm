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

(import (liii check) (liii gfproject tools))

(check-set-mode! 'report-failed)

;; 1. 同源路径不冲突
(check
  (gfproject-tool-conflict? "doc"
    '(("lib_path" . "/repo/goldfish/../gfproject.json")
      ("local_path" . "/repo/gfproject.json")
      ("lib_tools" ("doc" ("module" . "golddoc")))
      ("local_tools" ("doc" ("module" . "golddoc"))))
  ) ;gfproject-tool-conflict?
  =>
  #f
) ;check

;; 2. 真实已注册内置工具不冲突
(let ((bundle (gfproject-resolve-tool-bundle "doc")))
  (check (gfproject-tool-conflict? "doc" bundle) => #f)
) ;let

;; 3. 内置层与用户层同名、来源不同：冲突
(check
  (gfproject-tool-conflict? "doc"
    '(("lib_path" . "/repo/gfproject.json")
      ("user_path" . "/home/u/.gf/gfproject.json")
      ("local_path" . #f)
      ("lib_tools" ("doc" ("module" . "golddoc")))
      ("user_tools" ("doc" ("module" . "custom-doc")))
      ("local_tools" ()))
  ) ;gfproject-tool-conflict?
  =>
  #t
) ;check

;; 4. 内置层与项目层同名、来源不同：冲突
(check
  (gfproject-tool-conflict? "doc"
    '(("lib_path" . "/repo/gfproject.json")
      ("user_path" . #f)
      ("local_path" . "/work/gfproject.json")
      ("lib_tools" ("doc" ("module" . "golddoc")))
      ("user_tools" ())
      ("local_tools" ("doc" ("module" . "custom-doc"))))
  ) ;gfproject-tool-conflict?
  =>
  #t
) ;check

;; 5. 用户层与项目层同名不算冲突（项目层正常覆盖用户层）
(check
  (gfproject-tool-conflict? "custom"
    '(("lib_path" . "/repo/gfproject.json")
      ("user_path" . "/home/u/.gf/gfproject.json")
      ("local_path" . "/work/gfproject.json")
      ("lib_tools" ())
      ("user_tools" ("custom" ("module" . "user-custom")))
      ("local_tools" ("custom" ("module" . "local-custom"))))
  ) ;gfproject-tool-conflict?
  =>
  #f
) ;check

;; 6. 只有用户层注册：不冲突
(check
  (gfproject-tool-conflict? "custom"
    '(("lib_path" . "/repo/gfproject.json")
      ("user_path" . "/home/u/.gf/gfproject.json")
      ("local_path" . #f)
      ("lib_tools" ())
      ("user_tools" ("custom" ("module" . "user-custom")))
      ("local_tools" ()))
  ) ;gfproject-tool-conflict?
  =>
  #f
) ;check

(check-report)
