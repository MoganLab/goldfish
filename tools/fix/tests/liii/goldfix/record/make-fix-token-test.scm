;; make-fix-token
;; 创建带位置信息的 token 记录。
;;
;; 语法
;; ----
;; (make-fix-token (type 'other) (offset 0) (end 0) (line 1) (column 0) (text ""))
;;
;; 参数
;; ----
;; type : symbol? token 类型，如 open-paren / close-paren / other
;; offset : integer? token 在源码中的起始偏移量
;; end : integer? token 在源码中的结束偏移量
;; line : integer? token 所在的行号
;; column : integer? token 所在的列号
;; text : string? token 对应的源码文本
;;
;; 返回值
;; ------
;; fix-token 记录对象
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：创建 token 并读取各字段
(let ((token (make-fix-token :type 'open-paren :offset 3 :end 4 :line 2 :column 2 :text "(")
      ) ;token
     ) ;
  (check (fix-token? token) => #t)
  (check (fix-token-type token) => 'open-paren)
  (check (fix-token-offset token) => 3)
  (check (fix-token-end token) => 4)
  (check (fix-token-line token) => 2)
  (check (fix-token-column token) => 2)
  (check (fix-token-text token) => "(")
) ;let

;; 测试：全部使用缺省参数
(let ((token (make-fix-token)))
  (check (fix-token-type token) => 'other)
  (check (fix-token-offset token) => 0)
  (check (fix-token-end token) => 0)
  (check (fix-token-line token) => 1)
  (check (fix-token-column token) => 0)
  (check (fix-token-text token) => "")
) ;let

;; 测试：type 不是符号时报错
(check-catch 'value-error (make-fix-token :type "other"))

;; 测试：偏移量不是整数时报错
(check-catch 'value-error (make-fix-token :offset "0"))

;; 测试：偏移量为负数时报错
(check-catch 'value-error (make-fix-token :offset -1))

;; 测试：end 小于 offset 时报错
(check-catch 'value-error (make-fix-token :offset 5 :end 4))

;; 测试：text 不是字符串时报错
(check-catch 'value-error (make-fix-token :text 'x))

(check-report)
