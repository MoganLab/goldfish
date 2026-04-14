;; 添加 tools/goldtest 到 load path，以便导入 (liii goldtest)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/goldtest" *load-path*))

(import (liii check)
        (liii string)
        (liii path)
        (liii sys)
        (liii list)
        (liii os)
        (liii goldtest)
) ;import

;; ============================================================
;; parse-test-args 函数文档和测试
;; ============================================================
;;
;; 函数: parse-test-args
;; 用途: 解析 gf test 命令的参数，根据参数类型返回不同的匹配策略
;;
;; 参数:
;;   args - 命令行参数列表，第一个元素是可执行文件路径
;;
;; 返回值: (type . value) 对
;;   type 可以是:
;;     'file     - 直接运行指定文件
;;     'dir      - 运行目录下所有测试
;;     'filename - 按文件名精确匹配
;;     'pattern  - 按路径模糊匹配
;;     #f        - 无参数，运行所有测试
;;
;; 匹配规则（按优先级）:
;; 1. 包含 "/" 的路径参数:
;;    - path-file? 返回 #t  -> type='file, 直接运行该文件
;;    - path-dir? 返回 #t   -> type='dir, 运行目录下所有 *-test.scm
;;    - 不存在               -> type='pattern, 按路径模糊匹配
;;
;; 2. 以 ".scm" 结尾（但不包含 "/"）:
;;    -> type='filename, 按文件名精确匹配
;;    例: "json-test.scm" 匹配所有同名文件
;;
;; 3. 其他字符串:
;;    -> type='pattern, 按路径模糊匹配
;;    例: "json" 匹配路径中包含 "json" 的所有测试
;;
;; 特殊处理:
;; - 自动跳过 gf test 相关的命令选项 (-m, --mode, r7rs 等)
;; - 自动跳过第一个参数（可执行文件路径）

;; ------------------------------------------------------------
;; 测试用例
;; ------------------------------------------------------------

;; ===== 场景1: 无参数 =====
;; 当 args 只有可执行文件路径时，应该返回 (#f . #f)
(check (parse-test-args '("bin/gf")) => '(#f . #f))

;; ===== 场景2: 存在的文件路径 =====
;; 路径包含 / 且 path-file? 返回 #t -> type='file
(check (parse-test-args '("bin/gf" "tests/liii/json-test.scm"))
  => (if (path-file? "tests/liii/json-test.scm")
         '(file . "tests/liii/json-test.scm")
         ;; 如果文件不存在，按 pattern 处理
         '(pattern . "tests/liii/json-test.scm"))
) ;check

;; ===== 场景3: 存在的目录路径 =====
;; 路径包含 / 且 path-dir? 返回 #t -> type='dir
;; 返回原始路径（相对路径或绝对路径）
(check (parse-test-args '("bin/gf" "tests/liii/"))
  => (if (path-dir? "tests/liii/")
         '(dir . "tests/liii/")
         ;; 如果目录不存在，按 pattern 处理
         '(pattern . "tests/liii/"))
) ;check

(check (parse-test-args '("bin/gf" "tests/liii"))
  => (if (path-dir? "tests/liii")
         '(dir . "tests/liii")
         '(pattern . "tests/liii"))
) ;check

;; ===== 场景4: 不存在的路径 =====
;; 路径包含 / 但既不是文件也不是目录 -> type='pattern
(check (parse-test-args '("bin/gf" "nonexistent/path/test.scm"))
  => (if (or (path-file? "nonexistent/path/test.scm")
             (path-dir? "nonexistent/path/test.scm"))
         ;; 如果存在，按 file 或 dir 处理
         (if (path-file? "nonexistent/path/test.scm")
             '(file . "nonexistent/path/test.scm")
             '(dir . "nonexistent/path/test.scm")
         ) ;if
         ;; 如果不存在，按 pattern 处理
         '(pattern . "nonexistent/path/test.scm"))
) ;check

;; ===== 场景5: .scm 文件名（无路径） =====
;; 以 .scm 结尾但不包含 / -> type='filename
(check (parse-test-args '("bin/gf" "json-test.scm"))
  => '(filename . "json-test.scm")
) ;check

(check (parse-test-args '("bin/gf" "list-test.scm"))
  => '(filename . "list-test.scm")
) ;check

;; ===== 场景6: 普通字符串（模糊匹配） =====
;; 不包含 / 且不以 .scm 结尾 -> type='pattern
(check (parse-test-args '("bin/gf" "json"))
  => '(pattern . "json")
) ;check

(check (parse-test-args '("bin/gf" "liii"))
  => '(pattern . "liii")
) ;check

;; ===== 场景7: 跳过 test 命令本身 =====
(check (parse-test-args '("bin/gf" "test" "json"))
  => '(pattern . "json")
) ;check

;; ===== 场景8: 跳过 -m 和模式值 =====
(check (parse-test-args '("bin/gf" "-m" "r7rs" "json"))
  => '(pattern . "json")
) ;check

(check (parse-test-args '("bin/gf" "--mode" "liii" "json-test.scm"))
  => '(filename . "json-test.scm")
) ;check

;; ===== 场景9: 跳过 -m=... 格式 =====
(check (parse-test-args '("bin/gf" "-m=r7rs" "json"))
  => '(pattern . "json")
) ;check

;; ===== 场景10: 复杂命令行 =====
(check (parse-test-args '("bin/gf" "-m" "r7rs" "test" "tests/liii/json-test.scm"))
  => (if (path-file? "tests/liii/json-test.scm")
         '(file . "tests/liii/json-test.scm")
         '(pattern . "tests/liii/json-test.scm"))
) ;check

;; ===== 场景11: 带 ./ 的相对路径 =====
(check (parse-test-args '("bin/gf" "./tests/liii/json-test.scm"))
  => (if (path-file? "./tests/liii/json-test.scm")
         '(file . "./tests/liii/json-test.scm")
         '(pattern . "./tests/liii/json-test.scm"))
) ;check

;; ===== 场景12: 绝对路径 =====
(check (parse-test-args '("bin/gf" "/tmp/test.scm"))
  => (if (path-file? "/tmp/test.scm")
         '(file . "/tmp/test.scm")
         '(pattern . "/tmp/test.scm"))
) ;check

(check-report)
