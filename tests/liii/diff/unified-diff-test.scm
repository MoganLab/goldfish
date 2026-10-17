(import (liii check) (liii diff) (srfi srfi-13))

(check-set-mode! 'report-failed)

;; unified-diff
;; 生成修改前后的文本行列表的标准统一差异（Unified Diff）行列表。
;;
;; 语法
;; ----
;; (unified-diff from-lines to-lines [fromfile [tofile [fromfiledate [tofiledate [n]]]]])
;;
;; 参数
;; ----
;; from-lines : list of string
;; 修改前文本行列表。
;; to-lines : list of string
;; 修改后文本行列表。
;; fromfile : string (可选，默认 "a")
;; 差异头部原文件名标签。
;; tofile : string (可选，默认 "b")
;; 差异头部新文件名标签。
;; fromfiledate : string (可选，默认 "")
;; 差异头部原文件时间标签。
;; tofiledate : string (可选，默认 "")
;; 差异头部新文件时间标签。
;; n : exact-integer? (可选，默认 3)
;; 上下文保留行数，必须为非负整数。
;;
;; 返回值
;; -----
;; list of string
;; 符合标准统一差异规范的字符串列表；两份文本完全一致时返回空列表 '()。
;;
;; 错误处理
;; --------
;; type-error 当 from-lines 或 to-lines 不是字符串列表、文件名/时间不是字符串、或 n 不是精确整数时。
;; value-error 当 n 为负数时。

;; 测试项1：行差异比对生成标准 Unified Diff 补丁
(let* ((a '("def hello():" "    print('hello')"))
       (b '("def hello():" "    print('hello world')"))
       (diff (unified-diff a b "old.py" "new.py"))
      ) ;
  (check-true (list? diff))
  (check (length diff) => 6)
  (check (car diff) => "--- old.py")
  (check (cadr diff) => "+++ new.py")
  (check (caddr diff) => "@@ -1,2 +1,2 @@")
  (check (list-ref diff 3) => " def hello():")
  (check (list-ref diff 4) => "-    print('hello')")
  (check (list-ref diff 5) => "+    print('hello world')")
) ;let*

;; 测试项2：零差异空列表测试
(check (unified-diff '("same") '("same")) => '())
(check (unified-diff '() '()) => '())
(check (unified-diff '("a" "b" "c") '("a" "b" "c")) => '())

;; 测试项3：带日期的文件头测试
(let* ((a '("one"))
       (b '("two"))
       (diff (unified-diff a b "old.txt" "new.txt" "2024-01-01" "2024-01-02"))
      ) ;
  (check (car diff) => "--- old.txt\t2024-01-01")
  (check (cadr diff) => "+++ new.txt\t2024-01-02")
) ;let*

;; 测试项4：首尾与空输入边界测试
(let* ((empty-diff (unified-diff '() '("new line") "a" "b"))
       (del-diff (unified-diff '("old line") '() "a" "b"))
      ) ;
  (check (caddr empty-diff) => "@@ -0,0 +1 @@")
  (check (list-ref empty-diff 3) => "+new line")
  (check (caddr del-diff) => "@@ -1 +0,0 @@")
  (check (list-ref del-diff 3) => "-old line")
) ;let*

;; 测试项5：上下文行数 (n) 自定义与 Hunk 聚合测试
(let* ((a '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15"
            "16" "17" "18" "19" "20")
       ) ;a
       (b-close '("1" "2" "3_mod" "4" "5" "6_mod" "7" "8" "9" "10" "11" "12"
                  "13" "14" "15" "16" "17" "18" "19" "20")
       ) ;b-close
       (b-far '("1" "2_mod" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14"
                "15" "16" "17" "18_mod" "19" "20")
       ) ;b-far
       (count-hunks
         (lambda (lines)
           (let loop
             ((rem lines) (c 0))
             (if (null? rem)
               c
               (loop (cdr rem) (if (string-prefix? "@@" (car rem)) (+ c 1) c))
             ) ;if
           ) ;let
         ) ;lambda
       ) ;count-hunks
      ) ;
  ;; n=0: 仅包含改动行，无上下文
  (let ((diff-n0 (unified-diff a b-close "a" "b" "" "" 0)))
    (check-true (not (member " 1" diff-n0)))
    (check-true (not (not (member "-3" diff-n0))))
    (check-true (not (not (member "+3_mod" diff-n0))))
  ) ;let

  ;; 相距较近的改动合并为单一 Hunk (n=3)
  (let ((diff-close (unified-diff a b-close "a" "b" "" "" 3)))
    (check (count-hunks diff-close) => 1)
  ) ;let

  ;; 相距较远的改动分离为两个 Hunk (n=3)
  (let ((diff-far (unified-diff a b-far "a" "b" "" "" 3)))
    (check (count-hunks diff-far) => 2)
  ) ;let
) ;let*

;; 测试项6：参数校验与异常防御
(check-catch 'type-error (unified-diff "not-a-list" '()))
(check-catch 'type-error (unified-diff '() "not-a-list"))
(check-catch 'type-error (unified-diff '("valid") '(123)))
(check-catch 'type-error (unified-diff '(123) '("valid")))
(check-catch 'type-error (unified-diff '() '() 123))
(check-catch 'type-error (unified-diff '() '() "a" 456))
(check-catch 'type-error (unified-diff '() '() "a" "b" 1))
(check-catch 'type-error (unified-diff '() '() "a" "b" "" 2))
(check-catch 'type-error (unified-diff '() '() "a" "b" "" "" 3.5))
(check-catch 'value-error (unified-diff '() '() "a" "b" "" "" -1))

(check-report)
