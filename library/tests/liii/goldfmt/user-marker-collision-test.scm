;; user-marker-collision
;; 回归测试：issue 1468 —— 用户源码中的 (*comment* ...) / (*newline* ...) /
;; (*raw-string* ...) / (*char-literal* ...) 数据不能被当成 goldfmt 内部标记改写。
;;
;; 语法
;; ----
;; 无（本文件为整库回归测试，不测试单独导出函数）
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; 无（全部断言通过则测试通过）
;;
;; 说明
;; ----
;; 内部标记符号由 (liii goldfmt marker) 在库加载时生成一次性 nonce，
;; 用户源码在结构上不可能包含，因此用户数据按普通列表原样格式化输出。

(import (liii check)
  (liii goldfmt)
  (liii goldfmt marker)
)

(check-set-mode! 'report-failed)

;; 复现 1：quoted 数据 (*comment* "hello") 不被渲染成 ;; 注释
(check (format-string "(define e '(*comment* \"hello\"))\n")
  =>
  "(define e '(*comment* \"hello\"))\n"
) ;check

;; 复现 2：quoted 数据 (*newline* 2) 不被展开成空行（列表长度保持）
(check (format-string "(define e '(*newline* 2))\n")
  =>
  "(define e '(*newline* 2))\n"
) ;check

;; 嵌套列表中的 (*newline* n) 同样不被吞
(check (format-string "(define e '(a (*newline* 2) b))\n")
  =>
  "(define e '(a (*newline* 2) b))\n"
) ;check

;; raw-string 变体：用户数据 (*raw-string* ...) 不被折叠成裸字符串
(check (format-string "(define e '(*raw-string* \"x\" \"x\"))\n")
  =>
  "(define e '(*raw-string* \"x\" \"x\"))\n"
) ;check

;; char-literal 变体：用户数据 (*char-literal* ...) 不被折叠成字符
(check (format-string "(define e '(*char-literal* \"a\" #\\a))\n")
  =>
  "(define e '(*char-literal* \"a\" #\\a))\n"
) ;check

;; 输出可被 read 读回且数据保持：car 结果仍是符号 *comment*
(let* ((out (format-string "(define e '(*comment* \"hello\"))\n"))
       (back (read (open-input-string out)))
      ) ;
  (check back => '(define e (quote (*comment* "hello"))))
) ;let*

;; 输出可被 read 读回且数据保持：'(a (*newline* 2)) 长度仍为 2
(let* ((out (format-string "(define e '(a (*newline* 2)))\n"))
       (back (read (open-input-string out)))
      ) ;
  (check (length (cadr (caddr back))) => 2)
  (check (cadr (caddr back)) => '(a (*newline* 2)))
) ;let*

;; 真注释仍正常渲染：nonce 标记与用户数据互不影响
(check (format-string "(define e '(*comment* \"x\")) ;; real comment\n")
  =>
  "(define e '(*comment* \"x\"))\n;; real comment\n"
) ;check

;; 幂等性：格式化输出再格式化一次结果不变
(let* ((once (format-string "(define e '(*comment* \"hello\"))\n\n(define n '(a (*newline* 2) b))\n"))
       (twice (format-string once))
      ) ;
  (check twice => once)
) ;let*

;; 内部标记符号包含 nonce 前缀，且不等于用户可见的固定符号
(check (string-contains %comment-marker-tag-name "*goldfmt-comment-") => #t)
(check (string=? %comment-marker-tag-name "*comment*") => #f)
(check (string=? %newline-marker-tag-name "*newline*") => #f)
(check (string=? %raw-string-marker-tag-name "*raw-string*") => #f)
(check (string=? %char-literal-marker-tag-name "*char-literal*") => #f)

(check-report)
