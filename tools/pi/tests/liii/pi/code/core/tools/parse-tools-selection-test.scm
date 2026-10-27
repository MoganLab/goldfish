(import (liii check) (liii pi code core tools))

(check-set-mode! 'report-failed)

;; parse-tools-selection
;; 解析工具选择字符串，支持完整列表以及在基础工具集上增减工具。
;;
;; 语法
;; ----
;; (parse-tools-selection [spec [base-names]]) -> list
;;
;; 参数
;; ----
;; spec : string | #f
;; 逗号分隔的工具名；首项以 + 或 - 开头时按增量规则处理。
;; base-names : list
;; 可选的基础工具名列表，默认为 PI_DEFAULT_TOOL_NAMES。
;;
;; 返回值
;; ----
;; list
;; 工具名列表；+name 追加不存在的工具，-name 删除工具。
;; spec 为 #f 或空白字符串时返回 base-names。
;;
;; 示例
;; ----
;; (parse-tools-selection "+grep,-edit")
;; => '("read" "bash" "write" "grep")

(check (parse-tools-selection #f) => '("read" "bash" "edit" "write"))
(check (parse-tools-selection "read,grep") => '("read" "grep"))
(check (parse-tools-selection "+grep,-edit") => '("read" "bash" "write" "grep"))
(check (parse-tools-selection "+web_search")
  =>
  '("read" "bash" "edit" "write" "web_search")
) ;check
(check (parse-tools-selection "-web_search") => '("read" "bash" "edit" "write"))
(check (parse-tools-selection "web_search") => '("web_search"))
(check (parse-tools-selection "+grep" '("read" "bash" "edit" "write"
                                        "web_search"))
  =>
  '("read" "bash" "edit" "write" "web_search" "grep")
) ;check
(check (parse-tools-selection #f '("read" "web_search"))
  =>
  '("read" "web_search")
) ;check

(check-report)
