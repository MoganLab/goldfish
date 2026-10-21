(import (scheme base)
  (liii check)
  (liii pi extension install)
) ;import

(check (pi-parse-git-extension-name "https://github.com/org/pi-plugin.git") => "pi-plugin")
(check (pi-parse-git-extension-name "https://github.com/org/pi-plugin") => "pi-plugin")
(check (pi-parse-git-extension-name "https://github.com/org/pi-plugin/") => "pi-plugin")
(check (pi-parse-git-extension-name "https://github.com/org/pi-plugin.git/") => "pi-plugin")
(check (pi-parse-git-extension-name "git@github.com:org/pi-plugin.git") => "pi-plugin")
(check (pi-parse-git-extension-name "ssh://git@github.com/org/pi-plugin.git") => "pi-plugin")
(check (pi-parse-git-extension-name "/path/to/my-extension.git") => "my-extension")
(check (pi-parse-git-extension-name "my-extension") => "my-extension")
(check (pi-parse-git-extension-name "C:\\repos\\pi_super_tool.git") => "pi_super_tool")

;; 非法格式
(check (pi-parse-git-extension-name "") => #f)
(check (pi-parse-git-extension-name "   ") => #f)
(check (pi-parse-git-extension-name 123) => #f)

(check-report)
