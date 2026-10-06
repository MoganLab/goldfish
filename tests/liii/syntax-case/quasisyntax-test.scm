(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; quasisyntax
;; 构建准引用语法模板，允许在模板内部通过 unsyntax 与 unsyntax-splicing 插入计算值。
;;
;; 语法
;; ----
;; (quasisyntax template)
;;
;; 参数
;; ----
;; template : template
;; 准引用语法模板。
;;
;; 返回值
;; -----
;; syntax / any
;; 经 unsyntax 求值替换后的语法对象。

;; 1. 基础常数与字面量
(check (quasisyntax (+ 1 2)) => '(+ 1 2))

;; 2. 配合 unsyntax 进行求值插入
(check (quasisyntax (list (unsyntax (+ 1 2))))
       => '(list 3))

;; 3. 配合 unsyntax-splicing 进行列表解包拼接
(check (quasisyntax (list (unsyntax-splicing (list 1 2 3))))
       => '(list 1 2 3))

;; 4. 结合 syntax 模式变量与展开期计算
(check (syntax-case '(3 4) ()
         ((a b)
          (let ((sum (+ (syntax->datum (syntax a)) (syntax->datum (syntax b)))))
            (quasisyntax (sum-result (unsyntax sum))))))
       => '(sum-result 7))

(check-report)
