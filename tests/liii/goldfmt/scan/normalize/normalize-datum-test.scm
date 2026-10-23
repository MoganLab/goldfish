(import (liii check))
(import (liii goldfmt scan normalize)
        (liii goldfmt record))

(check-set-mode! 'report-failed)

;; normalize-datum
;; 将 read 读出的原始 datum 归一化：还原 s7 内部形式（quasiquote 展开结果），
;; 转换 reader 字面量标记，处理 dotted tail。
;;
;; 语法
;; ----
;; (normalize-datum datum)
;;
;; 参数
;; ----
;; datum : any?
;; read 读出的原始 datum。
;;
;; 返回值
;; ------
;; any?
;; 归一化后的 datum。
;;
;; 说明
;; ----
;; 1. (*raw-string* 源 值) 转换为 raw-string-literal 记录
;; 2. (*char-literal* 源 字符) 转换为 char-literal 记录
;; 3. s7 的 quasiquote 内部形式还原为 (quasiquote ...) 形式
;; 4. 普通 datum 原样返回

;; 测试：普通 datum 原样返回
(check (normalize-datum 42) => 42)
(check (normalize-datum 'sym) => 'sym)
(check (normalize-datum "str") => "str")

;; 测试：普通 list 递归归一化但结构不变
(check (normalize-datum '(+ 1 2)) => '(+ 1 2))

;; 测试：quote 形式保留
(check (normalize-datum '(quote (a b))) => '(quote (a b)))

;; 测试：vector 递归归一化
(check (normalize-datum #(1 2)) => #(1 2))

;; 测试：(*raw-string* ...) 转换为 raw-string-literal 记录
(let ((r (normalize-datum '(*raw-string* "#\"x\"" "x"))))
  (check (raw-string-literal? r) => #t)
  (check (raw-string-literal-value r) => "x")
) ;let

;; 测试：(*char-literal* ...) 转换为 char-literal 记录
(let ((r (normalize-datum '(*char-literal* "#\\a" #\a))))
  (check (char-literal? r) => #t)
  (check (char-literal-value r) => #\a)
) ;let

;; 测试：stem-mode 下 quote 保持 (quote x) 形式
(let ((r (call-with-stem-mode (lambda () (normalize-datum '(quote (a b)))))))
  (check r => '(quote (a b)))
) ;let

(check-report)
