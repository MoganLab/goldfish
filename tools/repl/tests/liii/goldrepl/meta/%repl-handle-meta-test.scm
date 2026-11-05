;; %repl-handle-meta
;; 分发以逗号开头的输入，返回 (退出标志 . 输出行列表)。
;;
;; 语法
;; ----
;; (%repl-handle-meta input)
;;
;; 参数
;; ----
;; input : string
;; 以逗号开头的元命令输入。
;;
;; 返回值
;; ----
;; list
;; 两个值：退出标志（#t 表示退出 REPL，'import 表示需要执行导入）与输出行列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl complete)
  (liii goldrepl meta)
) ;import

(check-set-mode! 'report-failed)

(%repl-update-symbol-cache!)

;; 未知元命令
(let-values (((quit? lines) (%repl-handle-meta ",xyz")))
  (check quit? => #f)
  (check-true (string-contains (car lines) "Unknown meta command"))
) ;let-values

;; 退出命令
(let-values (((quit? lines) (%repl-handle-meta ",quit")))
  (check quit? => #t)
  (check lines => '())
) ;let-values

;; 帮助命令
(let-values (((quit? lines) (%repl-handle-meta ",help")))
  (check quit? => #f)
  (check-true (pair? lines))
  (check-true (string-contains (car lines) "Meta commands:"))
) ;let-values

;; 导入命令
(let-values (((quit? lines) (%repl-handle-meta ",import (liii list)")))
  (check quit? => 'import)
  (check (car lines) => "(import (liii list))")
) ;let-values

;; 搜索命令
(let-values (((quit? lines) (%repl-handle-meta ",apropos string-app")))
  (check quit? => #f)
  (check-true (pair? lines))
  (check-true (any (lambda (l) (string-contains l "string-append")) lines))
) ;let-values

;; 描述命令
(let-values (((quit? lines) (%repl-handle-meta ",describe car")))
  (check quit? => #f)
  (check-true (pair? lines))
  (check-true (any (lambda (l) (string-contains l "Arity")) lines))
) ;let-values

(check-report)
