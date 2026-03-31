;; 添加 tools/goldeval 到 load path，以便导入 (liii goldeval)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/goldeval" *load-path*))

(import (liii check)
        (liii string)
        (liii list)
        (liii goldeval)
) ;import

;; ============================================================
;; parse-eval-args 函数文档和测试
;; ============================================================
;;
;; 函数: parse-eval-args
;; 用途: 解析 gf eval 命令的参数，返回模式和要执行的代码表达式
;;
;; 参数:
;;   args - 命令行参数列表，第一个元素是可执行文件路径
;;
;; 返回值: (mode . code-expr) 或 #f
;;   mode      - 模式字符串 ("liii", "r7rs" 等)
;;   code-expr - 要执行的代码表达式（多个表达式用 begin 包裹）
;;   #f        - 无代码参数
;;
;; 特殊处理:
;; - 自动跳过 gf eval 相关的命令选项 (-m, --mode 等)
;; - 自动跳过第一个参数（可执行文件路径）
;; - 多个代码参数用 (begin ...) 包裹

;; ------------------------------------------------------------
;; 测试用例
;; ------------------------------------------------------------

;; ===== 场景1: 无参数 =====
;; 当 args 只有可执行文件路径时，应该返回 #f
(check (parse-eval-args '("bin/gf")) => #f)

;; ===== 场景2: 单个表达式 =====
;; 默认模式为 "liii"
(check (parse-eval-args '("bin/gf" "(+ 1 2)")) => '("liii" . "(+ 1 2)"))

;; ===== 场景3: 跳过 eval 命令 =====
(check (parse-eval-args '("bin/gf" "eval" "(+ 1 2)")) => '("liii" . "(+ 1 2)"))

;; ===== 场景4: 解析 -m 和模式值 =====
(check (parse-eval-args '("bin/gf" "-m" "r7rs" "(+ 1 2)")) => '("r7rs" . "(+ 1 2)"))
(check (parse-eval-args '("bin/gf" "--mode" "sicp" "(+ 1 2)")) => '("sicp" . "(+ 1 2)"))

;; ===== 场景5: 解析 -m=... 格式 =====
(check (parse-eval-args '("bin/gf" "-m=r7rs" "(+ 1 2)")) => '("r7rs" . "(+ 1 2)"))
(check (parse-eval-args '("bin/gf" "--mode=sicp" "(+ 1 2)")) => '("sicp" . "(+ 1 2)"))

;; ===== 场景6: 多个表达式（用 begin 包裹） =====
(check (parse-eval-args '("bin/gf" "(+ 1 2)" "(* 3 4)"))
  => '("liii" . "(begin (+ 1 2) (* 3 4))")
) ;check

;; ===== 场景7: 跳过 -- 分隔符 =====
(check (parse-eval-args '("bin/gf" "--" "(+ 1 2)")) => '("liii" . "(+ 1 2)"))

;; ===== 场景8: 复杂命令行 =====
(check (parse-eval-args '("bin/gf" "-m" "r7rs" "eval" "(import (scheme base))" "(+ 1 2)"))
  => '("r7rs" . "(begin (import (scheme base)) (+ 1 2))")
) ;check

;; ===== 场景9: 带空格的表达式 =====
(check (parse-eval-args '("bin/gf" "(define x 10)")) => '("liii" . "(define x 10)"))

;; ===== 场景10: import 表达式 =====
(check (parse-eval-args '("bin/gf" "(import (liii string))" "(string-join '(\"a\" \"b\"))"))
  => '("liii" . "(begin (import (liii string)) (string-join '(\"a\" \"b\")))")
) ;check

;; ===== 场景11: 单参数多表达式（自动用 begin 包裹） =====
(check (parse-eval-args '("bin/gf" "(define x 1) (+ x 2)"))
  => '("liii" . "(begin (define x 1) (+ x 2))")
) ;check

(check-report)
