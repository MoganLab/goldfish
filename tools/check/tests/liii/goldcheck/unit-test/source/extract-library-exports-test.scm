;; extract-library-exports
;; 从 Scheme 库源码文件中提取全部导出的标识符名称列表。
;;
;; 语法
;; ----
;; (extract-library-exports source-path)
;;
;; 参数
;; ----
;; source-path : string
;; Scheme 库源码实现文件路径。
;;
;; 返回值
;; ------
;; list of string
;; 导出的标识符名称字符串列表。
;;
;; 说明
;; ----
;; 解析 (define-library ...) 结构中的 (export ...) 声明，支持普通符号导出及 (rename internal external) 重命名形式。

(import (scheme base)
  (liii check)
  (liii path)
  (liii goldcheck unit-test source)
) ;import

(check-set-mode! 'report-failed)

;; 1. 提取核心库导出标识符
(check (extract-library-exports "goldfish/scheme/case-lambda.scm") => '("case-lambda"))

;; 2. 临时库文件导出解析（含普通符号与 rename）
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-test-exports.scm")))
       (content (string-append
                  "(define-library (test lib)\n"
                  "  (export a (rename b c) d)\n"
                  "  (import (scheme base)))\n"
                ) ;string-append
       ) ;content
      ) ;let*
  (path-write-bytes (path tmp-file) (string->utf8 content))
  (check (extract-library-exports tmp-file) => '("a" "c" "d"))
  (path-unlink tmp-file #t)
) ;let*

(check-report)
