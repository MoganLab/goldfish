;; extract-library-reexported-entries
;; 从 Scheme 库源码文件中提取重导出的标识符点对列表（外部名称 . 内部名称）。
;;
;; 语法
;; ----
;; (extract-library-reexported-entries source-path)
;;
;; 参数
;; ----
;; source-path : string
;; Scheme 库源码实现文件路径。
;;
;; 返回值
;; ------
;; list of pair
;; 重导出条目的 (外部名称 . 内部名称) 点对列表。
;;
;; 说明
;; ----
;; 仅提取在库中导出但并未在本地定义的条目（即由外部库导入再导出的条目）。

(import (scheme base)
  (liii check)
  (liii path)
  (liii goldcheck unit-test source exports)
) ;import

(check-set-mode! 'report-failed)

;; 1. 混合本地定义与重导出
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-test-reexported-entries.scm"))
       ) ;tmp-file
       (content (string-append "(define-library (test lib)\n"
                  "  (export local-fn foreign-fn (rename orig-name alias-name))\n"
                  "  (import (other lib))\n" "  (begin\n"
                  "    (define (local-fn) 1)))\n"
                ) ;string-append
       ) ;content
      ) ;
  (path-write-bytes (path tmp-file) (string->utf8 content))
  (check (extract-library-reexported-entries tmp-file)
    =>
    '(("foreign-fn" . "foreign-fn") ("alias-name" . "orig-name"))
  ) ;check
  (path-unlink tmp-file #t)
) ;let*

;; 2. 无重导出的纯本地库返回空列表
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-test-no-reexport.scm"))
       ) ;tmp-file
       (content (string-append "(define-library (test lib2)\n"
                  "  (export local-fn)\n" "  (begin\n"
                  "    (define (local-fn) 1)))\n"
                ) ;string-append
       ) ;content
      ) ;
  (path-write-bytes (path tmp-file) (string->utf8 content))
  (check (extract-library-reexported-entries tmp-file) => '())
  (path-unlink tmp-file #t)
) ;let*

(check-report)
