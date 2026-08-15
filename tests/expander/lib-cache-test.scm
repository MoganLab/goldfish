(import (liii check) (liii string))

;; 阶段 2B 回归测试：define-library 缓存重建
;;
;; 验证链路：捕获 define-library 展开信息（bindings + 宏 spec + lower defs）
;; -> write-roundtrip 序列化 -> 读回 -> restore-library-cache 重建
;; -> import 后宏和值均可使用（缓存需与源码 mtime 匹配）。
;;
;; 注意：本测试依赖 load-library! 的缓存路径（bin/gf 已启用）。

;; ===== 1. 值库 + 宏库缓存重建 =====
;; srfi-2 是纯宏库；其宏展开引用 base 的 syntax-rules。
;; 先确保它已通过缓存加载，再验证宏可用。
(import (srfi srfi-2))
(check (and-let* ((x 5) (y (+ x 1))) (* x y)) => 30)

;; ===== 2. 依赖库场景（跨库 module-ref）=====
;; srfi-13 的值定义引用 liii base / srfi-1 等库的 binding，
;; 缓存重建后跨库引用（module-ref）需在 eval defs 前解析。
(import (srfi srfi-13))
(check (string-null? "") => #t)
(check (string-contains "hello world" "world") => #t)
(check (string-pad "abc" 5 #\*) => "**abc")

;; ===== 3. 复杂宏库（cond-expand 分支 + 内部辅助宏）=====
;; liii match 的 match-check-identifier 等定义在 cond-expand 的
;; 选中分支里，缓存提取必须展开 cond-expand 一步。
(import (liii match))
(check (match 'yes
         ((or 'yes 'no) 'boolean)
         (_ 'unknown))
       => 'boolean)
(check (match '(1 2 3)
         ((a b c) (list a b c))
         (_ #f))
       => '(1 2 3))

;; ===== 4. 值绑定 round-trip（toplevel-ref home 自引用）=====
;; 库值 binding 的 toplevel-ref home 指向库自身，缓存重建后
;; home 必须解析回重建的 exp-library（depurify-binding 处理）。
(import (liii base))
(check (string? "abc") => #t)

;; ===== 5. SRFI-17 广义 set!（liii logging 的 exit-hook 注册）=====
;; logging 库加载时执行 (set! (hook-functions *exit-hook*) ...) ——
;; SRFI-17 的 set! 位置子句。core-set! 展开该形式时曾误传
;; expand-list 三参数导致崩溃（8b258e42 修复），此处回归覆盖。
(import (liii logging))
(check (procedure? send-log) => #t)
(check (procedure? make-stdout-handler) => #t)
(check (string? (current-log-format)) => #t)

(check-report)
