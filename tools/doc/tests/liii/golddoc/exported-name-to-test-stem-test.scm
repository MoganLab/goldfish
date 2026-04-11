;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/golddoc" *load-path*))

(import (liii check)
        (liii golddoc)
) ;import

(check-set-mode! 'report-failed)

;; exported-name->test-stem
;; 将 Scheme 导出名转换为单元测试文件名中使用的 stem。
;;
;; 语法
;; ----
;; (exported-name->test-stem exported-name)
;;
;; 参数
;; ----
;; exported-name : string?
;; Scheme 中的导出过程名、谓词名或运算符名字。
;;
;; 返回值
;; ----
;; string?
;; 对应 `<stem>-test.scm` 里的 stem 部分。
;;
;; 描述
;; ----
;; 该函数遵循 `devel/Unit_Test_ZH.md` 中约定的命名规范，
;; 处理 `?`、`!`、`/`、`->`、`*` 以及比较运算符等转义规则。

(check (exported-name->test-stem "string-split") => "string-split")
(check (exported-name->test-stem "string->list") => "string-to-list")
(check (exported-name->test-stem "njson-set!") => "njson-set-bang")
(check (exported-name->test-stem "truncate/") => "truncate-slash")
(check (exported-name->test-stem "hash-table-update!/default") => "hash-table-update-bang-slash-default")
(check (exported-name->test-stem "trie-ref*") => "trie-ref-star")
(check (exported-name->test-stem "char-ci=?") => "char-ci-eq-p")
(check (exported-name->test-stem "vector=") => "vector-eq")
(check (exported-name->test-stem "isubset>=") => "isubset-ge")
(check (exported-name->test-stem "+") => "plus")
(check (exported-name->test-stem "-") => "minus")
(check (exported-name->test-stem "*") => "star")
(check (exported-name->test-stem "/") => "slash")
(check (exported-name->test-stem "=") => "eq")
(check (exported-name->test-stem "<") => "lt")
(check (exported-name->test-stem "<=") => "le")
(check (exported-name->test-stem ">") => "gt")
(check (exported-name->test-stem ">=") => "ge")

(check-report)
