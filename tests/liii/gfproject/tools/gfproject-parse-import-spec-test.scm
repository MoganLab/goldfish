;; gfproject-parse-import-spec
;; 将组织名与模块字符串转换为 Scheme 库导入规范。
;;
;; 语法
;; ----
;; (gfproject-parse-import-spec org module) -> list
;;
;; 参数
;; ----
;; org : string
;; 组织名称，如 "liii"。
;;
;; module : string
;; 模块标识，支持斜杠多级路径如 "pi/cli"。
;;
;; 返回值
;; ----
;; list
;; 解析出的符号列表导入形式，如 '(liii pi cli)。

(import (liii check)
  (liii gfproject tools)
) ;import

(check-set-mode! 'report-failed)

(check (gfproject-parse-import-spec "liii" "fmt") => '(liii fmt))
(check (gfproject-parse-import-spec "liii" "pi/cli") => '(liii pi cli))
(check (gfproject-parse-import-spec "liii" "/pi/cli/") => '(liii pi cli))
(check (gfproject-parse-import-spec "liii" "a/b/c") => '(liii a b c))
(check (gfproject-parse-import-spec "liii" "///pi///cli///") => '(liii pi cli))

(check-report)
