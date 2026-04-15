;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path*
  (cons "tools/golddoc" *load-path*)
) ;set!

(import (liii check)
  (liii golddoc)
  (liii path)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

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

(let ((doc-path (function-doc-path "liii/string"
                  "string-split"
                ) ;function-doc-path
      ) ;doc-path
     ) ;
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path)
    =>
    "string-split-test.scm"
  ) ;check
) ;let

(let ((doc-path (function-doc-path "scheme/base" "+")
      ) ;doc-path
     ) ;
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path)
    =>
    "plus-test.scm"
  ) ;check
) ;let

(let ((doc-path (function-doc-path "liii/njson"
                  "njson-set!"
                ) ;function-doc-path
      ) ;doc-path
     ) ;
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path)
    =>
    "njson-set-bang-test.scm"
  ) ;check
) ;let

(let ((doc-path (function-doc-path "scheme/base"
                  "truncate/"
                ) ;function-doc-path
      ) ;doc-path
     ) ;
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path)
    =>
    "truncate-slash-test.scm"
  ) ;check
) ;let

(let ((doc-path (function-doc-path "liii/hash-table"
                  "hash-table-update!/default"
                ) ;function-doc-path
      ) ;doc-path
     ) ;
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path)
    =>
    "hash-table-update-bang-slash-default-test.scm"
  ) ;check
) ;let

(let ((doc-path (function-doc-path "scheme/char"
                  "char-ci=?"
                ) ;function-doc-path
      ) ;doc-path
     ) ;
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path)
    =>
    "char-ci-eq-p-test.scm"
  ) ;check
) ;let

(let ((doc-path (function-doc-path "liii/option"
                  "option=?"
                ) ;function-doc-path
      ) ;doc-path
     ) ;
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path)
    =>
    "option-equal-p-test.scm"
  ) ;check
) ;let

(let ((doc-path (function-doc-path "liii/fxmapping"
                  "alist->fxmapping"
                ) ;function-doc-path
      ) ;doc-path
     ) ;
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path)
    =>
    "alist-fxmapping-test.scm"
  ) ;check
) ;let

(let ((doc-path (function-doc-path "liii/flexvector"
                  "flexvector-append-map/index"
                ) ;function-doc-path
      ) ;doc-path
     ) ;
  (check-true (string? doc-path))
  (check-true (path-file? doc-path))
  (check (path-name doc-path)
    =>
    "flexvector-append-map-index-test.scm"
  ) ;check
) ;let

(check (function-doc-path "liii/string"
         "not-a-real-function"
       ) ;function-doc-path
  =>
  #f
) ;check
(check (function-doc-path "liii/not-a-real-library"
         "string-split"
       ) ;function-doc-path
  =>
  #f
) ;check
(check (function-doc-path "srfi/1" "fold")
  =>
  #f
) ;check

(check-report)
