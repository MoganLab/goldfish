;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/golddoc" *load-path*))

(import (liii check)
        (liii golddoc)
        (liii path)
        (liii string)
) ;import

(check-set-mode! 'report-failed)

;; library-doc-path
;; 根据库查询字符串，结合当前 *load-path* 定位顶层库文档文件。
;;
;; 语法
;; ----
;; (library-doc-path query)
;;
;; 参数
;; ----
;; query : string?
;; 形如 "org/lib" 的库查询字符串。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 如果文档存在，返回对应 `<library>-test.scm` 的路径；否则返回 #f。
;;
;; 描述
;; ----
;; 该函数只处理当前 *load-path* 中可见的库，并且会排除 `srfi` 与
;; `goldfish` 目录下的测试文档。

(check (excluded-test-group? "srfi") => #t)
(check (excluded-test-group? "goldfish") => #t)
(check (excluded-test-group? "liii") => #f)

(let ((load-root (find-visible-library-root "liii/string")))
  (check-true (string? load-root))
  (check-true (path-file? (path-join load-root "liii" "string.scm")))
  (let ((tests-root (find-tests-root-for-load-root load-root)))
    (check-true (string? tests-root))
    (check-true (path-dir? tests-root))
    (check-true (path-file? (path-join tests-root "liii" "string-test.scm")))
  ) ;let
) ;let

(let ((doc-path (library-doc-path "liii/string")))
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path) => "string-test.scm")
) ;let

(check (library-doc-path "liii/not-a-real-library") => #f)
(check (library-doc-path "srfi/1") => #f)
(check (library-doc-path "goldfish/liii/http") => #f)

(check-report)
