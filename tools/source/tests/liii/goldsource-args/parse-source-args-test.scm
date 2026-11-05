;; parse-source-args
;; 解析 gf source 命令行参数，并识别帮助选项或单个库查询。
;;
;; 语法
;; ----
;; (parse-source-args args)
;;
;; 参数
;; ----
;; args : list?
;; 命令行参数字符串列表。
;;
;; 返回值
;; ------
;; list?
;; 解析结果列表，形如 '(library "组织/库名")、'(help) 或 '(invalid ...) 。

(import (scheme base) (liii check) (liii goldsource-args))

(check-set-mode! 'report-failed)

;; 测试：单个库查询参数
(check (parse-source-args '("bin/gf" "source" "liii/string"))
  =>
  '(library "liii/string")
) ;check

;; 测试：跳过 -m / --mode 选项
(check (parse-source-args '("bin/gf" "-m" "r7rs" "source" "liii/string"))
  =>
  '(library "liii/string")
) ;check

(check (parse-source-args '("bin/gf" "--mode=liii" "source" "liii/string"))
  =>
  '(library "liii/string")
) ;check

;; 测试：跳过 -I / -A 包含路径选项
(check (parse-source-args '("bin/gf" "-I" "/tmp" "-A" "/var/tmp" "source"
                            "liii/string")
       ) ;parse-source-args
  =>
  '(library "liii/string")
) ;check

;; 测试：--help 与 -h 选项
(check (parse-source-args '("bin/gf" "source" "--help")) => '(help))

(check (parse-source-args '("bin/gf" "source" "-h")) => '(help))

;; 测试：无参数返回 invalid
(check (parse-source-args '("bin/gf" "source")) => '(invalid))

;; 测试：非两段式库查询返回 invalid
(check (parse-source-args '("bin/gf" "source" "string-split"))
  =>
  '(invalid "string-split")
) ;check

;; 测试：多余参数返回 invalid
(check (parse-source-args '("bin/gf" "source" "liii/string" "extra"))
  =>
  '(invalid "liii/string" "extra")
) ;check

(check-report)
