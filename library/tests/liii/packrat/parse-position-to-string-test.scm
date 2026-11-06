;; parse-position->string
;; 把解析位置记录格式化为字符串。
;;
;; 语法
;; ----
;; (parse-position->string pos)
;;
;; 参数
;; ----
;; pos : <parse-position> 或 #f
;; 解析位置记录；为 #f 时输出占位串。
;;
;; 返回值
;; ----
;; string
;; 形如 "文件:行:列" 的位置描述；位置未知时为 "<??>"。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(check (parse-position->string (make-parse-position "test.scm" 3 15))
  =>
  "test.scm:3:15"
) ;check
(check (parse-position->string (top-parse-position "a.scm")) => "a.scm:1:0")

;; 位置未知
(check (parse-position->string #f) => "<??>")

(check-report)
