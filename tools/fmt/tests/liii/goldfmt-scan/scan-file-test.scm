(import (liii check))
(import (liii goldfmt-scan))
(import (liii goldfmt-record))
(import (liii path))
(import (liii vector))

(check-set-mode! 'report-failed)

;; 辅助函数：判断是否为注释节点
(define (comment-node? node)
  (and (env? node)
       (string=? (env-tag-name node) "*comment*")))

;; 辅助函数：判断是否为 define 节点
(define (define-node? node)
  (and (env? node)
       (string=? (env-tag-name node) "define")))

;; 测试 006_01.scm：基本注释处理
(let ((results (scan-file "tools/fmt/tests/resources/006_01.scm")))
  (check (vector? results) => #t)
  (check (vector-length results) => 5)  ; 3个注释 + 2个define
  ;; 使用 vector-count 统计注释数量
  (let ((comment-count (vector-count comment-node? results))
        (define-count (vector-count define-node? results)))
    (check comment-count => 3)  ; 3个注释
    (check define-count => 2))  ; 2个define
  ;; 检查第一个注释
  (check (env-tag-name (vector-ref results 0)) => "*comment*")
  ;; 检查第一个define
  (check (env-tag-name (vector-ref results 1)) => "define")
  ;; 检查第二个注释
  (check (env-tag-name (vector-ref results 2)) => "*comment*"))

;; 测试 006_02.scm：跨行注释应该被正确处理（忽略）
(let ((results (scan-file "tools/fmt/tests/resources/006_02.scm")))
  (check (vector? results) => #t)
  ;; 只有第一个行注释和 define（跨行注释被忽略）
  (let ((comment-count (vector-count comment-node? results))
        (define-count (vector-count define-node? results)))
    (check comment-count => 1)  ; 只有1个行注释
    (check define-count => 1))  ; 1个define
  (check (vector-length results) => 2))

;; 测试 006_03.scm：字符串中的分号不应被视为注释
(let ((results (scan-file "tools/fmt/tests/resources/006_03.scm")))
  (check (vector? results) => #t)
  ;; 第1个是注释，第2、3个是 define（字符串中的 ;; 不是注释）
  (let ((comment-count (vector-count comment-node? results))
        (define-count (vector-count define-node? results)))
    (check comment-count => 1)  ; 只有第1行是注释
    (check define-count => 2))  ; 2个define
  (check (vector-length results) => 3)
  ;; 验证顺序
  (check (env-tag-name (vector-ref results 0)) => "*comment*")
  (check (env-tag-name (vector-ref results 1)) => "define")
  (check (env-tag-name (vector-ref results 2)) => "define"))

(check-report)
