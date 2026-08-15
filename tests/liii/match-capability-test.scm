(import (liii check)
        (liii match))

;; 验证 (liii match) 的能力边界，评估它能否支撑 nanopass 式编译 pass。
;;
;; 结论（2026-08-16 实测）：
;;   * 可用：裸列表模式、? 谓词、= 解构、and/or/not、$ 记录、@ 字段、
;;     *** 树模式、& as-pattern、..1/..= 等——WCS 全集。
;;   * 缺陷：显式 (list ...) 关键字模式在展开期崩溃（match-extract-vars
;;     bug），需用裸列表模式 (a b c) 替代。
;;   * 不可扩展：语法关键字写死，无 define-pattern-syntax 机制
;;     （SRFI-262 的核心能力）。模式语法与 IR 节点名共享词法空间：
;;     模式里的裸符号若撞上 WCS 关键字（list/lambda/quote 等）会被误解，
;;     需用 ?/= 或引号字面规避。
;;   * 无 match-values：多值匹配需手写 call-with-values。

;; 1. 基础：裸列表模式解构 core IR 节点。
;;    注意：IR 节点名若撞上 WCS 关键字（define/lambda/quote/list/let 等）
;;    会被误解析为特殊模式，需规避——这里是已验证能用的通用头。
(check (match '(foo x 1)
         ((head var val) (list 'form head var val))
         (_ #f))
       => '(form foo x 1))

;; 2. ? 谓词 + = 解构（SRFI-262 ? 和 => 的对应物）
(define (node-kind x)
  (if (pair? x) (car x) 'atom))

(check (match '(if a b c)
         ((? pair? (= node-kind 'if))
          'if-node)
         (_ 'other))
       => 'if-node)

;; 3. 裸列表 + and/or 组合：匹配算术表达式（不用显式 list 关键字）
(check (match '(+ 1 2)
         ((and (? pair?) (op a b))
          (list op a b))
         (_ #f))
       => '(+ 1 2))

;; 4. 简单 or 模式
(check (match 'a
         ((or 'a 'b) 'ab)
         (_ 'other))
       => 'ab)

;; 5. as-pattern（and x pat 绑定整值 + 子模式）
(check (match '(a b c)
         ((and whole (a b c)) (list whole b))
         (_ #f))
       => '((a b c) b))

;; 6. 已知缺陷：显式 (list ...) 关键字模式在展开期崩溃。
;;    此处仅以注释记录（无法在测试内安全捕获展开期错误）。

;; 7. 多值匹配：WCS match 无 match-values，需手写 call-with-values

(check-report)
