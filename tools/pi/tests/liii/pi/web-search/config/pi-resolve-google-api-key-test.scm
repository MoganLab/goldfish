;; pi-resolve-google-api-key
;; 解析 Google 联网搜索使用的 API Key。
;;
;; 语法
;; ----
;; (pi-resolve-google-api-key config opt-key)
;;
;; 参数
;; ----
;; config : list?
;; 会话配置 alist，可包含 provider 与 api-key 字段。
;;
;; opt-key : (or string? boolean?)
;; 显式传入的 API Key。
;;
;; 返回值
;; ----
;; (or string? #f)
;; 生效的 API Key 字符串，未配置时返回 #f。

(import (liii check)
        (liii pi web-search config)
) ;import

(check-set-mode! 'report-failed)

;; 1. 显式传入的 API Key 优先
(check (pi-resolve-google-api-key '() "opt-key") => "opt-key")
(check (pi-resolve-google-api-key '() "  ") => #f) ; 空白 opt 视为未设置

;; 2. provider 为 google 时使用会话配置中的 api-key
(check (pi-resolve-google-api-key '((provider . "google") (api-key . "cfg-key")) #f)
  => "cfg-key"
) ;check

;; 3. 空白 opt 视为未设置
(check (pi-resolve-google-api-key '() "   ") => #f)

(check-report)
