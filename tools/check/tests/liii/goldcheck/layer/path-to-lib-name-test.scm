(import (scheme base) (liii check) (liii goldcheck layer))

;; path->lib-name
;; 按目录约定推导库名：goldfish/liii/a/b.scm -> "liii a b"；
;; tools/T/liii/a.scm -> "liii a"；非约定结构返回 #f。
;;
;; 语法
;; ----
;; (path->lib-name path)
;;
;; 参数
;; ----
;; path : string
;; 文件路径（相对或绝对，分隔符可为 / 或 \）。
;;
;; 返回值
;; ------
;; string 或 #f
;; 库名（段间以空格连接）或非库文件路径的 #f。

(check-set-mode! 'report-failed)

;; 1. goldfish 下的一级与二级库
(check (path->lib-name "goldfish/liii/string.scm") => "liii string")
(check (path->lib-name "goldfish/liii/format/base.scm") => "liii format base")
(check (path->lib-name "goldfish/srfi/srfi-1.scm") => "srfi srfi-1")

;; 2. tools 下的工具库
(check (path->lib-name "tools/check/liii/goldcheck.scm") => "liii goldcheck")
(check (path->lib-name "tools/pi/liii/pi/agent.scm") => "liii pi agent")

;; 3. 非约定结构返回 #f（含 tests 路径）
(check (path->lib-name "compat/char-numeric-p.scm") => #f)
(check (path->lib-name "tools/check/tests/liii/goldcheck/x.scm") => #f)

;; 4. 反斜杠分隔符归一化
(check (path->lib-name "goldfish\\liii\\string.scm") => "liii string")

;; 5. 非 .scm 文件返回 #f
(check (path->lib-name "goldfish/liii/string.txt") => #f)

;; 6. 绝对路径下同样定位结构锚点（临时目录模拟仓库结构）
(check
  (path->lib-name "C:/Users/u/AppData/Temp/gf/goldfish/srfi/srfi-1.scm")
  => "srfi srfi-1")
(check
  (path->lib-name "/tmp/gf/tools/check/liii/goldcheck.scm")
  => "liii goldcheck")

;; 7. liii 前一段不是 goldfish 且前两段不是 tools/T -> #f
(check (path->lib-name "foo/liii/bar.scm") => #f)
(check (path->lib-name "tools/check/tests/liii/x.scm") => #f)

(check-report)
