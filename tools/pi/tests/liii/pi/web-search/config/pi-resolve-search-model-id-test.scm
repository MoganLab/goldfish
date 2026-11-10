;; pi-resolve-search-model-id
;; 解析联网搜索使用的模型标识。
;;
;; 语法
;; ----
;; (pi-resolve-search-model-id config opt-model)
;;
;; 参数
;; ----
;; config : list?
;; 会话配置 alist，可包含 model-id 字段。
;;
;; opt-model : (or string? boolean?)
;; 显式传入的模型标识。
;;
;; 返回值
;; ----
;; string?
;; 生效的模型标识字符串，未配置时回退默认模型 "gemini-2.5-flash"。

(import (liii check) (liii pi web-search config))

(check-set-mode! 'report-failed)

;; 1. 显式传入的模型标识优先
(check (pi-resolve-search-model-id '() "gemini-3.8-flash")
  =>
  "gemini-3.8-flash"
) ;check
(check (pi-resolve-search-model-id '((model-id . "cfg-model")) "opt-model")
  =>
  "opt-model"
) ;check

;; 2. 未显式传入时使用会话配置
(check (pi-resolve-search-model-id '((model-id . "cfg-model")) #f)
  =>
  "cfg-model"
) ;check

;; 3. 配置缺失时回退默认模型
(check (pi-resolve-search-model-id '() #f) => "gemini-2.5-flash")

(check-report)
