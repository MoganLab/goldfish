(import (liii check)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code core extensions)
) ;import

(check-set-mode! 'report-failed)

;; resolve-active-tools
;; 综合默认工具、扩展声明、配置文件和命令行参数，计算实际启用的工具列表。
;;
;; 语法
;; ----
;; (resolve-active-tools parsed-args settings [registry]) -> list
;;
;; 参数
;; ----
;; parsed-args : alist
;; parse-pi-args 生成的命令行参数。
;; settings : alist
;; 包含 default-tools 的配置。
;; registry : extension-registry | extension-api | #f
;; 可选扩展注册表；缺省时使用当前或默认注册表。
;;
;; 返回值
;; ----
;; list
;; 去重且保持顺序的工具名列表。no-tools 优先返回空列表，exclude-tools 最后执行。
;;
;; 示例
;; ----
;; (resolve-active-tools parsed '((default-tools . ("read" "bash"))))
;; => '("read" "bash")

;; 工具增量配置：delta 基于内置默认 + 扩展 default-active 声明
(let* ((settings-with-delta '((default-tools "+grep" "-write")))
       (args-empty (parse-pi-args '()))
       (args-override (parse-pi-args '("--tools" "read,bash")))
       (api-ws (make-extension-api))
       (_
         (pi-load-extension (cond ((file-exists? "tools/pi/extensions/web-search.scm")
                                   "tools/pi/extensions/web-search.scm"
                                  ) ;
                                  ((file-exists? "extensions/web-search.scm") "extensions/web-search.scm")
                                  (else "../../extensions/web-search.scm")
                            ) ;cond
           api-ws
         ) ;pi-load-extension
       ) ;_
       (api-empty (make-extension-api))
      ) ;
  (check (resolve-active-tools args-empty settings-with-delta api-ws)
    =>
    '("read" "bash" "edit" "web_search" "grep")
  ) ;check
  (check (resolve-active-tools args-empty settings-with-delta api-empty)
    =>
    '("read" "bash" "edit" "grep")
  ) ;check
  (check (resolve-active-tools args-override settings-with-delta api-ws)
    =>
    '("read" "bash")
  ) ;check
) ;let*

;; --exclude-tools 排除测试与排除优先
(let ((args-ex (parse-pi-args '("--exclude-tools" "bash,write,edit")))
      (args-both (parse-pi-args '("--tools" "read,bash,edit" "--exclude-tools"
                                  "bash"))
      ) ;args-both
      (args-unknown (parse-pi-args '("--exclude-tools" "foo,bash")))
      (args-spaces (parse-pi-args '("--exclude-tools" " bash , edit ")))
      (settings '((default-tools "read" "bash" "edit" "write")))
     ) ;
  (check (resolve-active-tools args-ex settings) => '("read"))
  (check (resolve-active-tools args-both settings) => '("read" "edit"))
  (check (resolve-active-tools args-unknown settings) => '("read"
                                                           "edit"
                                                           "write"))
  (check (resolve-active-tools args-spaces settings) => '("read" "write"))
) ;let

;; --no-tools 与 --no-builtin-tools 支持与关闭优先
(let ((args-no-tools (parse-pi-args '("--no-tools")))
      (args-no-builtin (parse-pi-args '("--no-builtin-tools")))
      (args-override-tools (parse-pi-args '("--tools" "read,bash" "--no-tools")))
      (args-override-builtin (parse-pi-args '("--tools"
                                              "read,bash"
                                              "--no-builtin-tools"))
      ) ;args-override-builtin
      (args-override-exclude (parse-pi-args '("--exclude-tools"
                                              "bash"
                                              "--no-tools")))
      (args-override-all (parse-pi-args '("--tools" "read,bash"
                                          "--exclude-tools" "bash" "--no-tools"))
      ) ;args-override-all
      (settings '((default-tools "read" "bash" "edit" "write")))
     ) ;
  (check (resolve-active-tools args-no-tools settings) => '())
  (check (resolve-active-tools args-no-builtin settings) => '())
  (check (resolve-active-tools args-override-tools settings) => '())
  (check (resolve-active-tools args-override-builtin settings) => '())
  (check (resolve-active-tools args-override-exclude settings) => '())
  (check (resolve-active-tools args-override-all settings) => '())
) ;let

(check-report)
