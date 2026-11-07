;; parse-pi-args
;; 解析 Pi 命令行中的 simplify 参数。
;;
;; 语法
;; ----
;; (parse-pi-args argv)
;;
;; 参数
;; ----
;; argv : list
;; 不包含可执行文件名称的字符串参数列表。
;;
;; 返回值
;; ----
;; alist
;; 解析后的参数字典。

(import (liii check)
        (liii pi code cli args)
) ;import

(check-set-mode! 'report-failed)

;; pi-parse-args：simplify 相关开关
(let ((p (parse-pi-args '("--simplify"))))
  (check (pi-args-ref p 'simplify) => #t)
  (check (pi-args-ref p 'apply) => #f)
  (check (pi-args-ref p 'staged) => #f)
  (check (pi-args-ref p 'review-ref) => #f))

(let ((p (parse-pi-args '("--simplify" "--apply" "--staged" "--ref=main"
                          "src/a.ts" "src/b.ts"))))
  (check (pi-args-ref p 'simplify) => #t)
  (check (pi-args-ref p 'apply) => #t)
  (check (pi-args-ref p 'staged) => #t)
  (check (pi-args-ref p 'review-ref) => "main")
  (check (pi-args-ref p 'messages) => '("src/a.ts" "src/b.ts")))

;; 分离形式 --ref main
(let ((p (parse-pi-args '("--simplify" "--ref" "v1"))))
  (check (pi-args-ref p 'review-ref) => "v1")
  (check (pi-args-ref p 'messages) => '()))

;; 未知选项仍进入 unknown-flags
(let ((p (parse-pi-args '("--simplify" "--simplifier"))))
  (check (pi-args-ref p 'unknown-flags) => '("--simplifier")))

(check-report)
