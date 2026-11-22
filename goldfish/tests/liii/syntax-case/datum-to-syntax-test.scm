;; datum->syntax
;; 将普通 Scheme 原生数据（datum）转换为携带 template-id 词法作用域信息的语法对象。
;;
;; 语法
;; ----
;; (datum->syntax template-id datum)
;;
;; 参数
;; ----
;; template-id : syntax / identifier
;; 模板标识符或语法对象，用于提供词法作用域上下文环境。
;;
;; datum : any
;; 普通的 Scheme 数据结构（如符号、列表、常数等）。
;;
;; 返回值
;; -----
;; syntax
;; 包装了词法上下文环境信息的语法对象。

(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; 1. 符号与列表转换
(let ((stx (datum->syntax 'here '(foo 1 2))))
  (check (syntax->datum stx) => '(foo 1 2)))

;; 2. 标量值转换
(let ((stx (datum->syntax 'here 42)))
  (check (syntax->datum stx) => 42))

;; 3. 嵌套结构与向量转换
(let ((stx (datum->syntax 'here '#(a (b c)))))
  (check (syntax->datum stx) => '#(a (b c))))

;; 4. 配合宏定义顶层变量
(define-syntax def-x
  (lambda (stx)
    (syntax-case stx ()
      ((_ id val)
       (datum->syntax (syntax id)
         `(define ,(syntax->datum (syntax id)) ,(syntax->datum (syntax val))))))))
(def-x datum-stx-var 999)
(check datum-stx-var => 999)

;; 5. [1290] datum->syntax 产生的 define 目标必须与使用处引用同名：
;;    目标经 resolve 后应是裸符号，而不是 gensym 改名。
;;    展开后 datum-stx-var-2 在使用处直接可见。
(define-syntax def-x2
  (lambda (stx)
    (syntax-case stx ()
      ((_ id val)
       (datum->syntax (syntax id)
         `(define ,(syntax->datum (syntax id))
            (lambda () ,(syntax->datum (syntax val)))))))))
(def-x2 datum-stx-var-2 777)
(check (datum-stx-var-2) => 777)

(check-report)
