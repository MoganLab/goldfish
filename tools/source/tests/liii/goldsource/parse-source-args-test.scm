;; 添加 tools/goldsource 到 load path，以便导入 (liii goldsource)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path*
  (cons "tools/goldsource" *load-path*)
) ;set!

(import (liii check) (liii goldsource))

(check-set-mode! 'report-failed)

;; parse-source-args
;; 解析 gf source 命令行参数，并识别单个库查询。

(check (parse-source-args '("bin/gf" "source" "liii/string")
       ) ;parse-source-args
  =>
  '(library "liii/string")
) ;check

(check (parse-source-args '("bin/gf" "-m" "r7rs" "source" "liii/string")
       ) ;parse-source-args
  =>
  '(library "liii/string")
) ;check

(check (parse-source-args '("bin/gf" "-I" "/tmp" "-A" "/var/tmp" "source" "liii/string")
       ) ;parse-source-args
  =>
  '(library "liii/string")
) ;check

(check (parse-source-args '("bin/gf" "source"))
  =>
  '(invalid)
) ;check

(check (parse-source-args '("bin/gf" "source" "string-split")
       ) ;parse-source-args
  =>
  '(invalid "string-split")
) ;check

(check (parse-source-args '("bin/gf" "source" "liii/string" "extra")
       ) ;parse-source-args
  =>
  '(invalid "liii/string" "extra")
) ;check

(check-report)
