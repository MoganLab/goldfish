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
;; 1. (*goldfmt-raw-string-<nonce>* 源 值) 转换为 raw-string-literal 记录
;; 2. (*goldfmt-char-literal-<nonce>* 源 字符) 转换为 char-literal 记录
;; 3. s7 的 quasiquote 内部形式还原为 (quasiquote ...) 形式
;; 4. 普通 datum 原样返回

(import (liii check))
(import (liii goldfmt scan normalize) (liii goldfmt marker) (liii goldfmt record))

(check-set-mode! 'report-failed)


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

;; 测试：nonce raw-string 标记转换为 raw-string-literal 记录
;; 注意：不能直接写 '#"x" 字面量——按 srfi-267 语法，#"x 的分隔符是 x，
;; reader 会吞掉后续源码直到闭合的 "x"，导致本文件 reader 错位而加载失败。
(let ((r (normalize-datum (list %raw-string-marker-symbol "#\"x\"" "x"))))
  (check (raw-string-literal? r) => #t)
  (check (raw-string-literal-value r) => "x")
) ;let

;; 测试：nonce char-literal 标记转换为 char-literal 记录
(let ((r (normalize-datum (list %char-literal-marker-symbol "#\\a" #\a))))
  (check (char-literal? r) => #t)
  (check (char-literal-value r) => #\a)
) ;let

;; 测试：stem-mode 下 quote 保持 (quote x) 形式
(let ((r (call-with-stem-mode (lambda () (normalize-datum '(quote (a b)))))))
  (check r => '(quote (a b)))
) ;let

;; 测试：用户写的 (*raw-string* ...) / (*char-literal* ...) 是普通列表，不转换
(check (raw-string-literal? (normalize-datum '(*raw-string* "#\"\"" "x"))) => #f)
(check (char-literal? (normalize-datum '(*char-literal* "#\\a" #\a))) => #f)

(check-report)
