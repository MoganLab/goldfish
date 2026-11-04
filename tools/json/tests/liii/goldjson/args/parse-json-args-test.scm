;; parse-json-args
;; 解析 gf json 命令行参数为选项关联列表。
;;
;; 语法
;; ----
;; (parse-json-args args)
;;
;; 参数
;; ----
;; args : list?
;; 命令行参数字符串列表，如 '("-i" "4" "in.json" "out.json")。
;;
;; 返回值
;; -----
;; 关联列表或 #f
;; 成功时返回含 indent/compact/sort-keys/ensure-ascii/json-lines/validate/
;; help/positionals 八个键的关联列表；遇到非法参数时向标准错误输出提示
;; 并返回 #f。
;;
;; 说明
;; ----
;; 支持的选项：-i/--indent N、--indent=N、--tab、-c/--compact、
;; -s/--sort-keys、--ensure-ascii、--no-ensure-ascii、--json-lines、
;; -v/--validate、-h/--help；非选项参数按位置收集为 positionals。

(set! *load-path* (cons "tools/json" *load-path*))

(import (scheme base) (scheme write) (liii check) (liii goldjson args))

(check-set-mode! 'report-failed)

;; 1. 默认选项
(let ((p (parse-json-args '())))
  (check (cdr (assq 'indent p)) => 2)
  (check (cdr (assq 'compact p)) => #f)
  (check (cdr (assq 'sort-keys p)) => #f)
  (check (cdr (assq 'ensure-ascii p)) => #f)
  (check (cdr (assq 'json-lines p)) => #f)
  (check (cdr (assq 'validate p)) => #f)
  (check (cdr (assq 'help p)) => #f)
  (check (cdr (assq 'positionals p)) => '())
) ;let

;; 2. 显式选项
(let ((p (parse-json-args '("-i" "4" "-s" "--ensure-ascii" "in.json" "out.json"))))
  (check (cdr (assq 'indent p)) => 4)
  (check (cdr (assq 'sort-keys p)) => #t)
  (check (cdr (assq 'ensure-ascii p)) => #t)
  (check (cdr (assq 'positionals p)) => '("in.json" "out.json"))
) ;let

(let ((p (parse-json-args '("--indent=3" "--tab"))))
  ;; 后面的 --tab 覆盖 indent 为 "\t"
  (check (cdr (assq 'indent p)) => "\t")
) ;let

(let ((p (parse-json-args '("-c"))))
  (check (cdr (assq 'compact p)) => #t)
  (check (cdr (assq 'indent p)) => #f)
) ;let

(let ((p (parse-json-args '("--json-lines" "-v"))))
  (check (cdr (assq 'json-lines p)) => #t)
  (check (cdr (assq 'validate p)) => #t)
) ;let

(let ((p (parse-json-args '("-h"))))
  (check (cdr (assq 'help p)) => #t)
) ;let

;; 3. 非法参数返回 #f（错误提示重定向到字符串端口）
(let ((err (open-output-string)) (orig-err (current-error-port)))
  (set-current-error-port err)
  (check (parse-json-args '("-i")) => #f)
  (check (parse-json-args '("-i" "-1")) => #f)
  (check (parse-json-args '("-i" "abc")) => #f)
  (check (parse-json-args '("--indent=-5")) => #f)
  (check (parse-json-args '("--unrecognized-flag")) => #f)
  (set-current-error-port orig-err)
) ;let

(check-report)
