;; scan-source-string
;; 按原始源码扫描文本，保留注释与空行信息。
;;
;; 语法
;; ----
;; (scan-source-string raw-content)
;;
;; 参数
;; ----
;; raw-content : string?
;; 待扫描的原始源码文本。
;;
;; 返回值
;; ----
;; vector?
;; node 树组成的向量，注释与空行会被保留为对应的 node。
;;
;; 说明
;; ----
;; 1. 注释会被转换为 tag-name 为 "*comment*" 的 env 节点
;; 2. 连续空行会被转换为 tag-name 为 "*newline*" 的 env 节点
;; 3. 文本末尾的换行符同样会生成一个换行节点

(import (liii check) (liii goldfmt scan) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; 测试：空文本返回空向量
(check (vector? (scan-source-string "")) => #t)
(check (vector-length (scan-source-string "")) => 0)

;; 测试：单行注释被转换为 comment 节点
(let ((results (scan-source-string ";; hello")))
  (check (vector? results) => #t)
  (check (vector-length results) => 1)
  (check (env? (vector-ref results 0)) => #t)
  (check (env-tag-name (vector-ref results 0)) => "*comment*")
) ;let

;; 测试：注释与代码混合
(let ((results (scan-source-string ";; hello\n(define x 1)")))
  (check (vector-length results) => 2)
  (check (env-tag-name (vector-ref results 0)) => "*comment*")
  (check (env-tag-name (vector-ref results 1)) => "define")
) ;let

;; 测试：末尾换行生成 newline 节点
(let ((results (scan-source-string "(define x 1)\n")))
  (check (vector-length results) => 2)
  (check (env-tag-name (vector-ref results 0)) => "define")
  (check (env-tag-name (vector-ref results 1)) => "*newline*")
) ;let

;; 测试：连续空行合并为一个 newline 节点
(let ((results (scan-source-string "(define x 1)\n\n\n(define y 2)\n")))
  (check (vector-length results) => 4)
  (check (env-tag-name (vector-ref results 0)) => "define")
  (check (env-tag-name (vector-ref results 1)) => "*newline*")
  (check (env-tag-name (vector-ref results 2)) => "define")
  (check (env-tag-name (vector-ref results 3)) => "*newline*")
) ;let

;; 测试：开头的空行也会生成 newline 节点
(let ((results (scan-source-string "\n(define x 1)")))
  (check (vector-length results) => 2)
  (check (env-tag-name (vector-ref results 0)) => "*newline*")
  (check (env-tag-name (vector-ref results 1)) => "define")
) ;let

;; 测试：注释内容被保留在节点中
(let ((results (scan-source-string ";; hello\n")))
  (check (vector-length results) => 2)
  (check (car (env-value (vector-ref results 0))) => '*comment*)
  (check (cadr (env-value (vector-ref results 0))) => " hello")
) ;let

(check-report)
