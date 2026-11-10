;; parse-pi-args
;; 解析 Pi 命令行中的 review 参数。
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

(import (liii check) (liii pi code cli args))

(check-set-mode! 'report-failed)

;; pi-parse-args：code-review 相关开关
(let ((p (parse-pi-args '("--code-review"))))
  (check (pi-args-ref p 'code-review) => #t)
  (check (pi-args-ref p 'auto-review) => #f)
  (check (pi-args-ref p 'staged) => #f)
  (check (pi-args-ref p 'review-ref) => #f)
) ;let

(let ((p (parse-pi-args '("--code-review" "--staged" "--ref=main" "src/a.ts"
                          "src/b.py"))
      ) ;p
     ) ;
  (check (pi-args-ref p 'code-review) => #t)
  (check (pi-args-ref p 'staged) => #t)
  (check (pi-args-ref p 'review-ref) => "main")
  (check (pi-args-ref p 'messages) => '("src/a.ts" "src/b.py"))
) ;let

;; 分离形式 --ref main
(let ((p (parse-pi-args '("--code-review" "--ref" "v1"))))
  (check (pi-args-ref p 'review-ref) => "v1")
  (check (pi-args-ref p 'messages) => '())
) ;let

(let ((p (parse-pi-args '("--auto-review" "实现功能"))))
  (check (pi-args-ref p 'auto-review) => #t)
  (check (pi-args-ref p 'messages) => '("实现功能"))
) ;let

;; 未知选项仍进入 unknown-flags
(let ((p (parse-pi-args '("--refactor"))))
  (check (pi-args-ref p 'unknown-flags) => '("--refactor"))
) ;let

(check-report)
