(import (liii check)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code core extensions)
) ;import

(check-set-mode! 'report-failed)

;; 工具增量配置：delta 基于内置默认 + 扩展 default-active 声明
(let* ((settings-with-delta '((default-tools . ("+grep" "-write"))))
       (args-empty (parse-pi-args '()))
       (args-override (parse-pi-args '("--tools" "read,bash")))
       (api-ws (make-extension-api))
       (_ (pi-load-extension
            (cond
              ((file-exists? "tools/pi/extensions/web-search.scm")
               "tools/pi/extensions/web-search.scm")
              ((file-exists? "extensions/web-search.scm") "extensions/web-search.scm")
              (else "../../extensions/web-search.scm"))
            api-ws))
       (api-empty (make-extension-api)))
  (check (resolve-active-tools args-empty settings-with-delta api-ws)
    => '("read" "bash" "edit" "web_search" "grep"))
  (check (resolve-active-tools args-empty settings-with-delta api-empty)
    => '("read" "bash" "edit" "grep"))
  (check (resolve-active-tools args-override settings-with-delta api-ws)
    => '("read" "bash")))

;; --exclude-tools 排除测试与排除优先
(let ((args-ex (parse-pi-args '("--exclude-tools" "bash,write,edit")))
      (args-both (parse-pi-args '("--tools" "read,bash,edit" "--exclude-tools" "bash")))
      (args-unknown (parse-pi-args '("--exclude-tools" "foo,bash")))
      (args-spaces (parse-pi-args '("--exclude-tools" " bash , edit ")))
      (settings '((default-tools . ("read" "bash" "edit" "write")))))
  (check (resolve-active-tools args-ex settings) => '("read"))
  (check (resolve-active-tools args-both settings) => '("read" "edit"))
  (check (resolve-active-tools args-unknown settings) => '("read" "edit" "write"))
  (check (resolve-active-tools args-spaces settings) => '("read" "write")))

;; --no-tools 与 --no-builtin-tools 支持与关闭优先
(let ((args-no-tools (parse-pi-args '("--no-tools")))
      (args-no-builtin (parse-pi-args '("--no-builtin-tools")))
      (args-override-tools (parse-pi-args '("--tools" "read,bash" "--no-tools")))
      (args-override-builtin (parse-pi-args '("--tools" "read,bash" "--no-builtin-tools")))
      (args-override-exclude (parse-pi-args '("--exclude-tools" "bash" "--no-tools")))
      (args-override-all (parse-pi-args '("--tools" "read,bash" "--exclude-tools" "bash" "--no-tools")))
      (settings '((default-tools . ("read" "bash" "edit" "write")))))
  (check (resolve-active-tools args-no-tools settings) => '())
  (check (resolve-active-tools args-no-builtin settings) => '())
  (check (resolve-active-tools args-override-tools settings) => '())
  (check (resolve-active-tools args-override-builtin settings) => '())
  (check (resolve-active-tools args-override-exclude settings) => '())
  (check (resolve-active-tools args-override-all settings) => '()))

(check-report)
