;; gfproject-tool-base-dir
;; 返回工具配置所属的 gfproject.json 目录，用作相对 tools_dir 的基准。
;;
;; 语法
;; ----
;; (gfproject-tool-base-dir bundle) -> string | #f
;;
;; 参数
;; ----
;; bundle : alist
;; gfproject-resolve-tool-bundle 解析出的包字典。
;;
;; 返回值
;; ----
;; string 或 #f
;; 项目层覆盖 > 用户层 > 内置层，返回对应 gfproject.json 所在目录。

(import (liii check) (liii gfproject tools))

(check-set-mode! 'report-failed)

;; 项目层覆盖：以项目 gfproject.json 目录为基准
(check (gfproject-tool-base-dir '(("has-local-override" . #t)
                                  ("has-user-tool" . #t)
                                  ("lib_path" . "/gf/gfproject.json")
                                  ("user_path" . "/home/u/.gf/gfproject.json")
                                  ("local_path" . "/work/gfproject.json"))
       ) ;gfproject-tool-base-dir
  =>
  "/work"
) ;check

;; 只有用户层：以 ~/.gf 为基准（用户配置里可用相对路径）
(check (gfproject-tool-base-dir '(("has-local-override" . #f)
                                  ("has-user-tool" . #t)
                                  ("lib_path" . "/gf/gfproject.json")
                                  ("user_path" . "/home/u/.gf/gfproject.json")
                                  ("local_path" . #f))
       ) ;gfproject-tool-base-dir
  =>
  "/home/u/.gf"
) ;check

;; 内置层：以内置 gfproject.json 目录为基准
(check (gfproject-tool-base-dir '(("has-local-override" . #f)
                                  ("has-user-tool" . #f)
                                  ("lib_path" . "/gf/gfproject.json")
                                  ("user_path" . #f)
                                  ("local_path" . #f))
       ) ;gfproject-tool-base-dir
  =>
  "/gf"
) ;check

;; 没有来源文件时返回 #f
(check (gfproject-tool-base-dir '(("has-local-override" . #f)
                                  ("has-user-tool" . #f)
                                  ("lib_path" . #f)
                                  ("user_path" . #f)
                                  ("local_path" . #f))
       ) ;gfproject-tool-base-dir
  =>
  #f
) ;check

(check-report)
