;; extract-library-defined-exports
;; 从 Scheme 库源码文件中提取本地实现且导出的标识符名称列表。
;;
;; 语法
;; ----
;; (extract-library-defined-exports source-path)
;;
;; 参数
;; ----
;; source-path : string
;; Scheme 库源码实现文件路径。
;;
;; 返回值
;; ------
;; list of string
;; 本地实现且导出的标识符名称字符串列表。
;;
;; 说明
;; ----
;; 解析 (define-library ...) 结构中的 (export ...) 与 (begin ...) / (include ...)，
;; 仅提取在本地定义且导出的标识符，排除从其他库 import 再 export 的标识符。

(import (scheme base)
  (liii check)
  (liii path)
  (liii goldcheck unit-test source exports)
) ;import

(check-set-mode! 'report-failed)

;; 1. 只有本地定义的符号才被提取，从其他库 import 并 re-export 的符号被排除
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-test-defined-exports-1.scm"))
       ) ;tmp-file
       (content (string-append "(define-library (test lib)\n"
                  "  (export defined-fn reexported-fn (rename local-name ext-name))\n"
                  "  (import (other lib))\n" "  (begin\n"
                  "    (define (defined-fn x) x)\n"
                  "    (define local-name 123)))\n"
                ) ;string-append
       ) ;content
      ) ;
  (path-write-bytes (path tmp-file) (string->utf8 content))
  (check (extract-library-defined-exports tmp-file) => '("defined-fn"
                                                         "ext-name"))
  (path-unlink tmp-file #t)
) ;let*

;; 2. 支持多种定义形式：define, define*, define-syntax, define-macro
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-test-defined-exports-2.scm"))
       ) ;tmp-file
       (content (string-append "(define-library (test lib2)\n"
                  "  (export f1 f2 s1 m1 external-only)\n"
                  "  (import (scheme base))\n" "  (begin\n"
                  "    (define f1 1)\n" "    (define (f2 a b) (+ a b))\n"
                  "    (define-syntax s1 (syntax-rules ()))\n"
                  "    (define-macro (m1 x) x)))\n"
                ) ;string-append
       ) ;content
      ) ;
  (path-write-bytes (path tmp-file) (string->utf8 content))
  (check (extract-library-defined-exports tmp-file) => '("f1" "f2" "s1" "m1"))
  (path-unlink tmp-file #t)
) ;let*

;; 3. 全是 re-export 的库，本地导出为空
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-test-defined-exports-3.scm"))
       ) ;tmp-file
       (content (string-append "(define-library (test lib3)\n"
                  "  (export a b c)\n" "  (import (other lib))\n" "  (begin))\n"
                ) ;string-append
       ) ;content
      ) ;
  (path-write-bytes (path tmp-file) (string->utf8 content))
  (check (extract-library-defined-exports tmp-file) => '())
  (path-unlink tmp-file #t)
) ;let*

(check-report)
