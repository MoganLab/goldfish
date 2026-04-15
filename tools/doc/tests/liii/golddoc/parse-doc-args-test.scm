;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path*
  (cons "tools/golddoc" *load-path*)
) ;set!

(import (liii check) (liii golddoc))

(check-set-mode! 'report-failed)

;; parse-doc-args
;; 解析 gf doc 命令行参数，并区分库查询、库+函数查询与函数查询。
;;
;; 语法
;; ----
;; (parse-doc-args args)
;;
;; 参数
;; ----
;; args : list?
;; 完整命令行参数列表，第一个元素是可执行文件路径。
;;
;; 返回值
;; ----
;; list?
;; 返回形如 '(library query)、'(library-function library func)、
;; '(function func) 或 '(invalid ...) 的结构。
;;
;; 描述
;; ----
;; 该函数会跳过 `doc` 命令本身，以及 `-m`、`--mode`、`-I`、`-A`
;; 及其参数，便于后续 Scheme 层统一处理。

(check (parse-doc-args '("bin/gf" "doc" "liii/string")
       ) ;parse-doc-args
  =>
  '(library "liii/string")
) ;check

(check (parse-doc-args '("bin/gf" "-m" "r7rs" "doc" "liii/string")
       ) ;parse-doc-args
  =>
  '(library "liii/string")
) ;check

(check (parse-doc-args '("bin/gf" "-I" "/tmp" "-A" "/var/tmp" "doc" "liii/string")
       ) ;parse-doc-args
  =>
  '(library "liii/string")
) ;check

(check (parse-doc-args '("bin/gf" "doc" "liii/string" "string-split")
       ) ;parse-doc-args
  =>
  '(library-function "liii/string" "string-split")
) ;check

(check (parse-doc-args '("bin/gf" "doc" "string-split")
       ) ;parse-doc-args
  =>
  '(function "string-split")
) ;check

(check (parse-doc-args '("bin/gf" "doc" "--build-json")
       ) ;parse-doc-args
  =>
  '(build-json)
) ;check

(check (parse-doc-args '("bin/gf" "doc" "truncate/")
       ) ;parse-doc-args
  =>
  '(function "truncate/")
) ;check

(check (parse-doc-args '("bin/gf" "doc" "alist->fxmapping/combinator")
       ) ;parse-doc-args
  =>
  '(library "alist->fxmapping/combinator")
) ;check

(check (parse-doc-args '("bin/gf" "doc"))
  =>
  '(invalid)
) ;check

(check (parse-doc-args '("bin/gf" "doc" "liii/string" "string-split" "extra")
       ) ;parse-doc-args
  =>
  '(invalid "liii/string" "string-split" "extra")
) ;check

(check-report)
