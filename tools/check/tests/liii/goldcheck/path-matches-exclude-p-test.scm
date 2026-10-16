(import (scheme base) (liii check) (liii goldcheck-config))

;; path-matches-exclude?
;; 判断待测文件路径是否命中单个排除模式（支持通配符 glob、目录前缀及精确匹配）。
;;
;; 语法
;; ----
;; (path-matches-exclude? entry-str pattern)
;;
;; 参数
;; ----
;; entry-str : string
;; 待检查的文件相对或绝对路径。
;; pattern : string
;; 豁免规则路径模式（如目录前缀 "goldfish/srfi/"、通配符 "*.test.scm"、精确路径 "foo.scm"）。
;;
;; 返回值
;; ------
;; boolean
;; 命中规则返回 #t，否则返回 #f。

(check-set-mode! 'report-failed)

;; 1. 目录前缀匹配（以 / 结尾）
(check-true (path-matches-exclude? "goldfish/srfi/1.scm" "goldfish/srfi/"))
(check-true (path-matches-exclude? "./goldfish/srfi/1.scm" "goldfish/srfi/"))
(check-false (path-matches-exclude? "goldfish/liii/os.scm" "goldfish/srfi/"))

;; 2. 精确路径匹配与相对子路径匹配
(check-true (path-matches-exclude? "goldfish/liii/goldfmt/format.scm"
              "goldfish/liii/goldfmt/format.scm"
            ) ;path-matches-exclude?
) ;check-true
(check-true (path-matches-exclude? "goldfish/liii/goldfmt/format.scm" "goldfmt/format.scm")
) ;check-true
(check-false (path-matches-exclude? "goldfish/liii/goldfmt/scan.scm" "goldfmt/format.scm")
) ;check-false

;; 3. 通配符 glob 匹配
(check-true (path-matches-exclude? "src/s7.c" "src/s7*"))
(check-true (path-matches-exclude? "src/s7_r7rs.c" "src/s7*"))
(check-false (path-matches-exclude? "src/goldfish.cpp" "src/s7*"))
(check-true (path-matches-exclude? "foo/bar/baz.scm" "*.scm"))

(check-report)
