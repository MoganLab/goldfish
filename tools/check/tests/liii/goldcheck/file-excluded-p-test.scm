(import (scheme base) (liii check) (liii goldcheck-config))

;; file-excluded?
;; 判断待测文件路径是否命中 exclude 列表中的任一条规则。
;;
;; 语法
;; ----
;; (file-excluded? entry-str excludes)
;;
;; 参数
;; ----
;; entry-str : string
;; 待检查的文件路径。
;; excludes : list
;; 豁免规则列表，元素可以是字符串或对象。
;;
;; 返回值
;; ------
;; boolean
;; 命中任意一条规则返回 #t，全部未命中返回 #f。

(check-set-mode! 'report-failed)

(let ((excludes (list "goldfish/srfi/"
                  '((path . "goldfish/liii/go.scm") (reason . "协程"))
                  "src/s7*"
                ) ;list
      ) ;excludes
     ) ;
  (check-true (file-excluded? "goldfish/srfi/srfi-1.scm" excludes))
  (check-true (file-excluded? "goldfish/liii/go.scm" excludes))
  (check-true (file-excluded? "src/s7.h" excludes))
  (check-false (file-excluded? "goldfish/liii/os.scm" excludes))
) ;let

(check-report)
