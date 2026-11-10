;; function-doc-path
;; 根据库查询字符串和导出名，定位对应叶子测试文档文件。
;;
;; 语法
;; ----
;; (function-doc-path library-query exported-name)
;;
;; 参数
;; ----
;; library-query : string?
;; 形如 "org/lib" 的库查询字符串。
;;
;; exported-name : string?
;; Scheme 导出过程名、谓词名或运算符名。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 如果对应叶子测试文档存在，则返回其路径；否则返回 #f。
;;
;; 描述
;; ----
;; 该函数在当前 *load-path* 中查找可见库，再将导出名按统一规则
;; 转换为测试文件 stem，并定位：
;; `tests/<group>/<library>/<stem>-test.scm`

;; 添加 tools/doc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/doc" *load-path*))

(import (liii check) (liii golddoc) (liii path) (liii string))

(check-set-mode! 'report-failed)

(let ((doc-path (function-doc-path "liii/string" "string-split")))
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path) => "string-split-test.scm")
) ;let

(let ((doc-path (function-doc-path "scheme/base" "+")))
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path) => "plus-test.scm")
) ;let

(let ((doc-path (function-doc-path "scheme/base" "vector-set!")))
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path) => "vector-set-bang-test.scm")
) ;let

(let ((doc-path (function-doc-path "scheme/base" "truncate/")))
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path) => "truncate-slash-test.scm")
) ;let

(let ((doc-path (function-doc-path "liii/alist" "alist-ref/default")))
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path) => "alist-ref-slash-default-test.scm")
) ;let

;; (liii hash-table) 仅重导出 (srfi srfi-125)，其叶子文档位于
;; tests/srfi 分组，不在 gf doc 支持范围内，返回 #f
(check (function-doc-path "liii/hash-table" "hash-table-set!") => #f)

(let ((doc-path (function-doc-path "scheme/char" "char-ci=?")))
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path) => "char-ci-eq-p-test.scm")
) ;let

(let ((doc-path (function-doc-path "liii/option" "option=?")))
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path) => "option-eq-p-test.scm")
) ;let

;; (liii fxmapping) 仅重导出 (srfi srfi-224)，其叶子文档位于
;; tests/srfi 分组，不在 gf doc 支持范围内，返回 #f
(check (function-doc-path "liii/fxmapping" "alist->fxmapping") => #f)

;; (liii flexvector) 仅重导出 (srfi srfi-214)，其叶子文档位于
;; tests/srfi 分组，不在 gf doc 支持范围内，返回 #f
(check (function-doc-path "liii/flexvector" "flexvector-set!") => #f)

(check (function-doc-path "liii/string" "not-a-real-function") => #f)
(check (function-doc-path "liii/not-a-real-library" "string-split") => #f)
(check (function-doc-path "srfi/1" "fold") => #f)

(let ((doc-path (function-doc-path "liii/uri/compare" "uri=?")))
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path) => "uri-eq-p-test.scm")
) ;let

(let ((doc-path (function-doc-path "liii/uri/compare" "uri-hash")))
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path) => "uri-hash-test.scm")
) ;let

(let ((doc-path (function-doc-path "liii/uri" "uri=?")))
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path) => "uri-eq-p-test.scm")
) ;let

(let ((doc-path (function-doc-path "liii/uri" "uri-hash")))
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path) => "uri-hash-test.scm")
) ;let

(check (function-doc-path "liii/uri/compare" "not-a-real-function") => #f)

(check-report)
