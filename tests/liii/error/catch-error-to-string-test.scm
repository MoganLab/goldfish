;; catch-error->string
;; 综合格式化 catch 捕获到的异常信息，包含消息展开、位置与提示。
;;
;; 语法
;; ----
;; (catch-error->string tag info) -> string
;;
;; 参数
;; ----
;; tag : symbol or any
;; catch 捕获到的错误标签。
;;
;; info : list or any
;; catch 传递给错误处理器的参数。
;;
;; 返回值
;; ----
;; string
;; 格式化后的完整错误提示字符串。

(import (liii check)
  (liii string)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; 1. 普通纯字符串错误
(check (catch-error->string 'user-error '("bad argument")) => "bad argument")
(check (catch-error->string 'user-error "bad argument direct") => "bad argument direct")

;; 2. 带占位符的嵌套格式化错误展开
(let ((res (catch-error->string 'type-error '(("expected ~S but got ~S" integer "abc")))))
  (check res => "expected integer but got \"abc\"")
) ;let

;; 3. 三层嵌套结构的展开（lambda (tag . info) 场景）
(let ((res (catch-error->string 'type-error '((("expected ~S but got ~S" integer "abc"))))))
  (check res => "expected integer but got \"abc\"")
) ;let

;; 4. 未绑定变量错误包含展开的符号名与代码片段
(let ((res (catch-error->string 'unbound-variable
             '(("unbound variable ~S in ~S" unknown-symbol (+ unknown-symbol 1)))
           ) ;catch-error->string
      ) ;res
     ) ;
  (check-true (string-contains? res "unbound variable unknown-symbol in (+ unknown-symbol 1)"))
) ;let

;; 5. 端到端真实异常测试：未绑定变量自动输出错误信息、代码位置与 import Hint
(let ((res (catch #t
             (lambda () (eval '(read-json "{}") (rootlet)))
             (lambda (tag . info) (catch-error->string tag info))
           ) ;catch
      ) ;res
     ) ;
  (check-true (string? res))
  (check-true (string-contains? res "unbound variable read-json"))
  (check-true (string-contains? res "Hint: `read-json` exists in library:"))
  (check-true (string-contains? res "(import (liii json))"))
) ;let

(check-report)
