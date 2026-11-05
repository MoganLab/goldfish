;; make-fix-line
;; 创建按物理行组织后的 token 行记录。
;;
;; 语法
;; ----
;; (make-fix-line (number 1) (start-offset 0) (first-code-token #f) (tokens '()))
;;
;; 参数
;; ----
;; number : integer? 行号，从 1 开始
;; start-offset : integer? 该行在源码中的起始偏移量
;; first-code-token : fix-token? 该行第一枚 code token，无则为 #f
;; tokens : list? 该行的 token 列表
;;
;; 返回值
;; ------
;; fix-line 记录对象
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：创建行记录并读取各字段
(let* ((token (make-fix-token :type 'other :offset 0 :end 6 :line 1 :column 0 :text "define")
       ) ;token
       (line (make-fix-line
               :number           1
               :start-offset     0
               :first-code-token token
               :tokens           (list token)
             ) ;make-fix-line
       ) ;line
      ) ;
  (check (fix-line? line) => #t)
  (check (fix-line-number line) => 1)
  (check (fix-line-start-offset line) => 0)
  (check (fix-line-first-code-token line) => token)
  (check (fix-line-tokens line) => (list token))
) ;let*

;; 测试：缺省参数
(let ((line (make-fix-line)))
  (check (fix-line-number line) => 1)
  (check (fix-line-start-offset line) => 0)
  (check (fix-line-first-code-token line) => #f)
  (check (fix-line-tokens line) => '())
) ;let

;; 测试：行号不是整数时报错
(check-catch 'value-error (make-fix-line :number "1"))

;; 测试：first-code-token 不是 fix-token 时报错
(check-catch 'value-error (make-fix-line :first-code-token 'x))

;; 测试：tokens 不是列表时报错
(check-catch 'value-error (make-fix-line :tokens 'x))

(check-report)
