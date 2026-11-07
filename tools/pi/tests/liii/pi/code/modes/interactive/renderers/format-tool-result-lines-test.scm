;; format-tool-result-lines
;; format-tool-result-lines 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (format-tool-result-lines ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base)
  (liii check)
  (liii list)
  (liii pi code modes interactive renderers)
) ;import

(check-set-mode! 'report-failed)

;; 简单单行
(check (format-tool-result-lines "bash" "Hello World") => '("Hello World"))

;; 短多行（<= 5 行完整保留）
(let ((lines (format-tool-result-lines "bash" "line1\nline2\nline3")))
  (check lines => '("line1" "line2" "line3"))
) ;let

;; bash 超长（如 8 行）：默认保留尾部 5 行，并在首部追加折叠提示
(let* ((text "1\n2\n3\n4\n5\n6\n7\n8")
       (lines (format-tool-result-lines "bash" text :max-lines 5))
      ) ;
  (check (length lines) => 6)
  (check (car lines) => "... (3 earlier lines, ctrl+o to expand)")
  (check (cdr lines) => '("4" "5" "6" "7" "8"))
) ;let*

;; read/grep 等超长（如 8 行）：默认保留头部 5 行，尾部追加折叠提示
;; 提示文案对齐上游 createResultFallback："... (N more lines, ctrl+o to expand)"
(let* ((text "1\n2\n3\n4\n5\n6\n7\n8")
       (lines (format-tool-result-lines "read" text :max-lines 5))
      ) ;
  (check (length lines) => 6)
  (check (list-ref lines 5) => "... (3 more lines, ctrl+o to expand)")
  (check (take lines 5) => '("1" "2" "3" "4" "5"))
) ;let*

;; 结构化结果（如 pi-tool-result 或 inlet 包含 content）提取测试
(let* ((mock-inlet (inlet 'is-error #f 'content #(((type . "text") (text . "ddd\n")))))
       (lines (format-tool-result-lines "bash" mock-inlet))
      ) ;
  (check lines => '("ddd"))
) ;let*

;; ============================================================
;; 对齐上游 picodeagent：expanded? 作为独立选项控制展开/折叠，
;; 折叠预览行数按工具区分（read=0 write=10 grep=15 find=20 ls=20 具体见 views 层）
;; ============================================================

;; 展开态：全文输出，无截断提示
(let* ((text "1\n2\n3\n4\n5\n6\n7\n8")
       (lines (format-tool-result-lines "read" text :expanded? #t))
      ) ;
  (check lines => '("1" "2" "3" "4" "5" "6" "7" "8"))
) ;let*

;; 展开态显式 max-lines 仍被忽略（expanded? 优先）
(let* ((text "1\n2\n3\n4\n5\n6\n7\n8")
       (lines (format-tool-result-lines "bash" text :max-lines 5 :expanded? #t))
      ) ;
  (check lines => '("1" "2" "3" "4" "5" "6" "7" "8"))
) ;let*

;; 展开态空结果与折叠态一致返回空表
(check (format-tool-result-lines "read" "" :expanded? #t) => '())

;; 所有工具输出都按上游 getTextOutput 清理：RIS、关闭鼠标和颜色序列
;; 不得进入卡片渲染，避免不可信工具结果改变宿主终端状态。
(check (format-tool-result-lines "read" "before\x1b;cafter" :expanded? #t)
  =>
  '("beforeafter")
) ;check
(check (format-tool-result-lines "grep" "before\x1b;[?1000lafter")
  =>
  '("beforeafter")
) ;check
(check (format-tool-result-lines "custom" "\x1b;[31mred\x1b;[0m\x01;")
  =>
  '("red")
) ;check
;; 与 Bash 显示管线一致：TAB 展开为三空格，CR 删除。
(check (format-tool-result-lines "read" "a\tb\r\n") => '("a   b"))

(check-report)
