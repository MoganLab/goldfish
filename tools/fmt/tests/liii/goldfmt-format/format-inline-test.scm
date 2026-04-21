(import (liii check)
  (liii goldfmt-format)
  (liii goldfmt-scan)
) ;import

(check-set-mode! 'report-failed)

;; format-inline
;; 将 node 转成单行候选字符串。
;;
;; 语法
;; ----
;; (format-inline node)
;;
;; 参数
;; ----
;; node : env? 或 atom?
;; 由 `scan` 生成的格式化节点。
;;
;; 返回值
;; ------
;; string?
;; 返回 node 的单行字符串表示。
;;
;; 说明
;; ----
;; 1. `format-inline` 只计算候选文本，不记录位置信息
;; 2. `format-inline` 不修改传入的 node
;; 3. 该函数主要给 `can-inline?` 做长度判定使用
;; 4. 空 tag-name 的 env 不会在第一个 child 前额外输出空格
;;
;; 示例
;; ----
;; (format-inline (scan '(+ x y))) ; => "(+ x y)"

(check (format-inline (scan '(+ x y)))
  =>
  "(+ x y)"
) ;check

(check (format-inline (scan 'x)) => "x")

(check (format-inline (scan "hello"))
  =>
  "\"hello\""
) ;check

(check (format-inline (scan #t))
  =>
  "#t"
) ;check

(check (format-inline (scan '()))
  =>
  "()"
) ;check

(check (format-inline (scan '#(1 2)))
  =>
  "#(1 2)"
) ;check

(check (format-inline (scan '((x 1) (y 2))))
  =>
  "((x 1) (y 2))"
) ;check

(check (format-inline (scan '(quote (1 2 3))))
  =>
  "'(1 2 3)"
) ;check

(check (format-inline (scan '(*comment* "hello"))
       ) ;format-inline
  =>
  ";; hello"
) ;check

(check (format-inline (scan '(*comment* "")))
  =>
  ";;"
) ;check

(check (format-inline (scan '(*comment* "包含 \"quote\" 和 \\ slash")
                      ) ;scan
       ) ;format-inline
  =>
  ";; 包含 \"quote\" 和 \\ slash"
) ;check

(check-report)
