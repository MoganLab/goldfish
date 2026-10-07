;; (liii syntax-case) 过程式宏系统模块函数分类索引
;;
;; `(liii syntax-case)` 提供 R6RS 风格的过程式卫生宏扩展机制，
;; 派生自 Chibi Scheme (chibi syntax-case)，作者 Marc Nieper-Wißkirchen。
;;
;; ==== 使用说明 ====
;;   (import (liii syntax-case))
;;
;; ==== 查看函数与宏文档 ====
;;   bin/gf doc liii/syntax-case
;;   bin/gf doc syntax-case
;;   bin/gf doc syntax
;;   bin/gf doc quasisyntax
;;   bin/gf doc with-syntax
;;   bin/gf doc datum->syntax
;;   bin/gf doc syntax->datum
;;
;; ==== 函数与宏分类索引 ====
;;
;; 一、模式匹配与转录核心宏
;;   syntax-case           - 模式匹配语法对象并执行展开期任意计算过程
;;   syntax                - 构建保留词法上下文的语法对象模板
;;   quasisyntax           - 语法准引用模板，支持 unsyntax 嵌入
;;   unsyntax              - 准引用内部求值注入
;;   unsyntax-splicing     - 准引用内部列表平铺注入
;;   with-syntax           - 局部模式变量绑定
;;   with-ellipsis         - 指定局部自定义省略号关键字
;;
;; 二、语法对象与原生数据互转
;;   datum->syntax         - 将原生 S 表达式包装为携带词法作用域的语法对象
;;   syntax->datum         - 递归剥除语法闭包还原为原生 S 表达式
;;
;; 三、标识符操作与谓词
;;   identifier?           - 判断是否为标识符
;;   free-identifier=?     - 比较两标识符在各自环境中的有效绑定
;;   bound-identifier=?    - 比较两标识符是否同名且具有相同绑定身份
;;   ellipsis-identifier?  - 判断是否为省略号标识符 ...
;;   generate-temporaries  - 生成不冲突的唯一临时标识符列表
;;   syntax-violation      - 抛出结构化语法错误异常

(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; 1. syntax-case 基础匹配与过程式计算宏
(define-syntax calc-add
  (lambda (stx)
    (syntax-case stx ()
      ((_ a b)
       (let ((sum (+ (syntax->datum (syntax a))
                     (syntax->datum (syntax b)))))
         (quasisyntax (list (unsyntax sum))))))))

(check (calc-add 10 20) => '(30))

;; 2. 局部 with-syntax 绑定与重排
(check (with-syntax (((a b) '(10 20)))
         (quasisyntax (sum (unsyntax (+ (syntax a) (syntax b))))))
       => '(sum 30))

;; 3. 语法对象与原生数据互转
(let ((stx (datum->syntax 'here '(foo 1 2))))
  (check (syntax->datum stx) => '(foo 1 2)))

;; 4. 标识符谓词
(check-true (free-identifier=? 'a 'a))
(check-false (free-identifier=? 'a 'b))
(check-true (bound-identifier=? 'a 'a))
(check-false (bound-identifier=? 'a 'b))
(check-true (identifier? 'a))
(check-false (identifier? 123))

;; 5. 跨库宏调用的卫生隔离
(define-library (test private-syntax-case-helper)
  (export exported-sc-macro)
  (import (scheme base) (liii syntax-case))
  (begin
    (define (private-sc-add10 x) (+ x 10))
    (define-syntax exported-sc-macro
      (lambda (stx)
        (syntax-case stx ()
          ((_ x) (syntax (private-sc-add10 x))))))))

(import (test private-syntax-case-helper))
(check (exported-sc-macro 5) => 15)
(check-catch 'unbound-variable (private-sc-add10 5))

;; 6. 验证 syntax-case 定义的宏在多次 GC 之后展开仍然正常
(define-syntax my-pair-macro
  (lambda (stx)
    (syntax-case stx ()
      ((_ (fn arg ...)) (syntax (fn arg ...))))))

(let loop ((i 0))
  (when (< i 10)
    (gc)
    (loop (+ i 1))))

(check (my-pair-macro (+ 1 2 3)) => 6)

;; 7. 统计生成 symbol 数量
(let* ((c0 (%syntax-case-counter))
       (_ (eval '(syntax-case '(+ 1 2) () ((_ a b) (syntax (+ a b)))) (curlet)))
       (c1 (%syntax-case-counter)))
  ;; 展开单次简单的二元运算匹配，仅生成 7 个内部局部临时符号
  (check (- c1 c0) => 7))

(check-report)
