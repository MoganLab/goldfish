(import (scheme base) (liii check) (liii pi code core extensions install))

(check-set-mode! 'report-failed)

;; pi-parse-git-extension-name
;; 从 Git URL、SSH 地址或本地路径推断扩展目录名称。
;;
;; 语法
;; ----
;; (pi-parse-git-extension-name url) -> string | #f
;;
;; 参数
;; ----
;; url : string
;; 仓库地址或路径，支持 /、\\、: 分隔以及末尾 .git 和斜杠。
;;
;; 返回值
;; ----
;; string | #f
;; 返回最后一个路径段并去除末尾 .git；非字符串或空白输入返回 #f。
;;
;; 示例
;; ----
;; (pi-parse-git-extension-name "git@github.com:org/pi-plugin.git")
;; => "pi-plugin"

(check (pi-parse-git-extension-name "https://github.com/org/pi-plugin.git")
  =>
  "pi-plugin"
) ;check
(check (pi-parse-git-extension-name "https://github.com/org/pi-plugin")
  =>
  "pi-plugin"
) ;check
(check (pi-parse-git-extension-name "https://github.com/org/pi-plugin/")
  =>
  "pi-plugin"
) ;check
(check (pi-parse-git-extension-name "https://github.com/org/pi-plugin.git/")
  =>
  "pi-plugin"
) ;check
(check (pi-parse-git-extension-name "git@github.com:org/pi-plugin.git")
  =>
  "pi-plugin"
) ;check
(check (pi-parse-git-extension-name "ssh://git@github.com/org/pi-plugin.git")
  =>
  "pi-plugin"
) ;check
(check (pi-parse-git-extension-name "/path/to/my-extension.git")
  =>
  "my-extension"
) ;check
(check (pi-parse-git-extension-name "my-extension") => "my-extension")
(check (pi-parse-git-extension-name "C:\\repos\\pi_super_tool.git")
  =>
  "pi_super_tool"
) ;check

;; 非法格式
(check (pi-parse-git-extension-name "") => #f)
(check (pi-parse-git-extension-name "   ") => #f)
(check (pi-parse-git-extension-name 123) => #f)

(check-report)
