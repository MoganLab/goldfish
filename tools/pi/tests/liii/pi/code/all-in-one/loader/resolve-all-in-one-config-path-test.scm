;; resolve-all-in-one-config-path
;; 解析 all-in-one.json 的完整文件路径。
;;
;; 语法
;; ----
;; (resolve-all-in-one-config-path . opt-config-dir)
;;
;; 参数
;; ----
;; opt-config-dir : (or string path #f) (可选)
;; 可选配置目录，未提供或为 #f 时返回默认的 ~/.gf/agent/all-in-one.json。
;;
;; 返回值
;; ----
;; string
;; all-in-one.json 文件的完整路径字符串。

(import (scheme base) (liii check) (liii path) (liii pi code all-in-one loader))

(check-set-mode! 'report-failed)

;; 1. 默认路径
(check (resolve-all-in-one-config-path)
  =>
  (path->string (path-join (path-home) ".gf" "agent" "all-in-one.json"))
) ;check

;; 2. 显式传入 #f
(check (resolve-all-in-one-config-path #f)
  =>
  (path->string (path-join (path-home) ".gf" "agent" "all-in-one.json"))
) ;check

;; 3. 显式传入配置目录
(check (resolve-all-in-one-config-path "/tmp/gf-test")
  =>
  (path->string (path-join "/tmp/gf-test" "all-in-one.json"))
) ;check

(check-report)
