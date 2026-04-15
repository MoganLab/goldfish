(import (liii check)
        (liii goldfmt-scan)
        (liii goldfmt-record)
        (liii path)
        (liii vector)
        (liii os))

(check-set-mode! 'report-failed)

;; 辅助函数：获取资源文件路径
;; 优先使用相对于 tools/fmt 的路径，如果不存在则使用绝对路径
(define (resource-file filename)
  (let ((local-path (string-append "tests/resources/" filename))
        (abs-path (string-append "tools/fmt/tests/resources/" filename)))
    (if (access local-path 'R_OK)
        local-path
        abs-path)))

;; 辅助函数：判断是否为注释节点
(define (comment-node? node)
  (and (env? node)
       (string=? (env-tag-name node) "*comment*")))

;; 辅助函数：判断是否为 define 节点
(define (define-node? node)
  (and (env? node)
       (string=? (env-tag-name node) "define")))

;; 辅助函数：判断是否为 newline 节点
(define (newline-node? node)
  (and (env? node)
       (string=? (env-tag-name node) "*newline*")))

;; 测试 006_01.scm：基本注释处理
(let ((results (scan-file (resource-file "006_01.scm"))))
  (check (vector? results) => #t)
  (check (vector-length results) => 6)  ; 3个注释 + 2个define + 1个文件末尾空行
  ;; 使用 vector-count 统计各类节点数量
  (let ((comment-count (vector-count comment-node? results))
        (define-count (vector-count define-node? results))
        (newline-count (vector-count newline-node? results)))
    (check comment-count => 3)  ; 3个注释
    (check define-count => 2)   ; 2个define
    (check newline-count => 1)) ; 1个空行（在文件末尾）
  ;; 验证顺序：注释-define-注释-define-注释-空行
  (check (env-tag-name (vector-ref results 0)) => "*comment*")
  (check (env-tag-name (vector-ref results 1)) => "define")
  (check (env-tag-name (vector-ref results 2)) => "*comment*")
  (check (env-tag-name (vector-ref results 3)) => "define")
  (check (env-tag-name (vector-ref results 4)) => "*comment*")
  (check (env-tag-name (vector-ref results 5)) => "*newline*"))

;; 测试 006_02.scm：跨行注释应该被正确处理（忽略）
(let ((results (scan-file (resource-file "006_02.scm"))))
  (check (vector? results) => #t)
  ;; block comment 内容被忽略，但会留下一个 count=1 的换行占位
  (let ((comment-count (vector-count comment-node? results))
        (define-count (vector-count define-node? results))
        (newline-count (vector-count newline-node? results)))
    (check comment-count => 1)  ; 只有1个行注释
    (check define-count => 1)   ; 1个define
    (check newline-count => 2)) ; block comment 占位 + 文件末尾空行
  (check (vector-length results) => 4)
  ;; 验证顺序
  (check (env-tag-name (vector-ref results 0)) => "*comment*")
  (check (env-tag-name (vector-ref results 1)) => "*newline*")
  (check (env-tag-name (vector-ref results 2)) => "define")
  (check (env-tag-name (vector-ref results 3)) => "*newline*"))

;; 测试 006_03.scm：字符串中的分号不应被视为注释
(let ((results (scan-file (resource-file "006_03.scm"))))
  (check (vector? results) => #t)
  ;; 第1个是注释，第2、3个是 define，最后1个是文件末尾空行（字符串中的 ;; 不是注释）
  (let ((comment-count (vector-count comment-node? results))
        (define-count (vector-count define-node? results))
        (newline-count (vector-count newline-node? results)))
    (check comment-count => 1)  ; 只有第1行是注释
    (check define-count => 2)   ; 2个define
    (check newline-count => 1)) ; 1个文件末尾空行
  (check (vector-length results) => 4)
  ;; 验证顺序
  (check (env-tag-name (vector-ref results 0)) => "*comment*")
  (check (env-tag-name (vector-ref results 1)) => "define")
  (check (env-tag-name (vector-ref results 2)) => "define")
  (check (env-tag-name (vector-ref results 3)) => "*newline*"))

;; 测试 006_04.scm：raw string 中以 ;; 开头的行不应被拆成注释
(let ((results (scan-file (resource-file "006_04.scm"))))
  (check (vector? results) => #t)
  (check (vector-length results) => 3)
  (let ((first (vector-ref results 0))
        (second (vector-ref results 1))
        (third (vector-ref results 2)))
    (check (env-tag-name first) => "define")
    (check (env-tag-name second) => "define")
    (check (env-tag-name third) => "*newline*")
    (let* ((value-node (vector-ref (env-children first) 1))
           (literal (atom-value value-node)))
      (check (atom? value-node) => #t)
      (check (raw-string-literal? literal) => #t)
      (check (raw-string-literal-source literal)
             => "#\"SQL\"\n  ;; not a comment\n  SELECT 1\n  \"SQL\"")
      (check (raw-string-literal-value literal)
             => "\n  ;; not a comment\n  SELECT 1\n  "))))

;; 测试 006_05.scm：空 delimiter 的 raw string 结束后，后续注释不应丢失
(let ((results (scan-file (resource-file "006_05.scm"))))
  (check (vector? results) => #t)
  (check (vector-length results) => 4)
  (let ((first (vector-ref results 0))
        (second (vector-ref results 1))
        (third (vector-ref results 2))
        (fourth (vector-ref results 3)))
    (check (env-tag-name first) => "define")
    (check (env-tag-name second) => "*comment*")
    (check (env-tag-name third) => "define")
    (check (env-tag-name fourth) => "*newline*")
    (let ((content (vector-ref (env-children second) 0)))
      (check (atom? content) => #t)
      (check (atom-value content) => " raw string 后的注释"))))

(check-report)
