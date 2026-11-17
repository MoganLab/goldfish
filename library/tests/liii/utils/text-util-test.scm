(set! *load-path* (cons "markdown/goldfish" *load-path*))

;; (liii cmark text) 字符串通用工具测试
;;
;; text-util 提供 markdown 插件共用的字符串/字符位置原语；
;; 本文件覆盖新增通用导出：string-index-of / char-at? / string-at?。

;; ==== 常见用法示例 ====
(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report)

;; string-index-of
;; 从 start 起查找子串 sub 的起始索引；找不到返回 #f
;;
;; 语法
;; ----
;; (string-index-of s sub start)

(let ((result (string-index-of "hello world" "world" 0)))
  (check result => 6)
) ;let

(let ((result (string-index-of "hello world" "o" 0)))
  (check result => 4)
) ;let

(let ((result (string-index-of "hello world" "xyz" 0)))
  (check result => #f)
) ;let

;; start 偏移：跳过前面命中
(let ((result (string-index-of "aaa" "a" 1)))
  (check result => 1)
) ;let

;; start 越过末尾
(let ((result (string-index-of "hello" "h" 10)))
  (check result => #f)
) ;let

;; 空子串：任何位置都可匹配（此处按实现：i+n <= len 才查）
(let ((result (string-index-of "abc" "" 0)))
  (check result => 0)
) ;let

;; char-at?
;; s 位置 pos 是否为字符 ch（越界返回 #f）
;;
;; 语法
;; ----
;; (char-at? s pos len ch)

(let ((result (char-at? "abc" 1 3 #\b)))
  (check result => #t)
) ;let

(let ((result (char-at? "abc" 0 3 #\b)))
  (check result => #f)
) ;let

;; 越界保护：pos = len 不崩溃，返回 #f
(let ((result (char-at? "abc" 3 3 #\c)))
  (check result => #f)
) ;let

(let ((result (char-at? "abc" 9 3 #\a)))
  (check result => #f)
) ;let

;; 空串：len 0，任何 pos 都越界
(let ((result (char-at? "" 0 0 #\a)))
  (check result => #f)
) ;let

;; 换行字符（math-parser 用）
(let ((result (char-at? "a\nb" 1 3 #\newline)))
  (check result => #t)
) ;let

;; string-at?
;; s 从 pos 起匹配子串 sub（越界返回 #f）
;;
;; 语法
;; ----
;; (string-at? s pos sub)

(let ((result (string-at? "hello" 0 "he")))
  (check result => #t)
) ;let

(let ((result (string-at? "hello" 2 "llo")))
  (check result => #t)
) ;let

;; 子串越界（起点合法但长度超出）
(let ((result (string-at? "hello" 4 "llo")))
  (check result => #f)
) ;let

;; 负位置
(let ((result (string-at? "hello" -1 "h")))
  (check result => #f)
) ;let

;; 不匹配
(let ((result (string-at? "hello" 0 "xx")))
  (check result => #f)
) ;let

;; 空子串匹配任意合法位置
(let ((result (string-at? "hello" 3 "")))
  (check result => #t)
) ;let

;; prepend-rev
;; 返回 reverse(l) ++ acc（尾共享 acc，不深拷贝；供反复拼接增长结果时避免 append 深拷贝 O(n²)）
;;
;; 语法
;; ----
;; (prepend-rev l acc)

(let ((result (prepend-rev '(1 2) '())))
  (check result => '(2 1))
) ;let

(let ((result (prepend-rev '(3) '(1 2))))
  (check result => '(3 1 2))
) ;let

;; 尾共享：acc 不被拷贝（prepend-rev 只把 l 的翻转载到 acc 前）
(let ((acc '(a b)) (l '(1)))
  (check (equal? (prepend-rev l acc) '(1 a b)) => #t)
) ;let

;; stable-sort
;; 按比较器 lt 稳定归并排序（O(n log n)，乱序/反序不回退 O(n²)）
;;
;; 语法
;; ----
;; (stable-sort xs lt)

(let ((result (stable-sort '() <)))
  (check result => '())
) ;let

(let ((result (stable-sort '(7) <)))
  (check result => '(7))
) ;let

(let ((result (stable-sort '(5 3 1 4 2) <)))
  (check result => '(1 2 3 4 5))
) ;let

;; 重复键：稳定（同键保持原序）
(let ((result
        (stable-sort (list '(1 . a) '(1 . b) '(0 . c))
          (lambda (x y) (< (car x) (car y)))
        ) ;stable-sort
      ) ;result
     ) ;
  (check (equal? result (list '(0 . c) '(1 . a) '(1 . b))) => #t)
) ;let

;; 字符串比较
(let ((result (stable-sort '("b" "a" "c") string<?)))
  (check result => '("a" "b" "c"))
) ;let

;; 反序输入不回退 O(n²)（功能验证：结果正确）
(let ((result (stable-sort '(9 8 7 6 5 4 3 2 1) <)))
  (check result => '(1 2 3 4 5 6 7 8 9))
) ;let

;; restore-math-greek-herk
;; 将数学容器内 utf8->herk 产生的 <#3C6>/<#3D5>/<#3D6> 还原为 <varphi>/<phi>/<varpi>
;;
;; 语法
;; ----
;; (restore-math-greek-herk s)

(check (restore-math-greek-herk "<#3C6>") => "<varphi>")
(check (restore-math-greek-herk "<#03C6>") => "<varphi>")
(check (restore-math-greek-herk "<#3D5>") => "<phi>")
(check (restore-math-greek-herk "<#03D5>") => "<phi>")
(check (restore-math-greek-herk "<#3D6>") => "<varpi>")
(check (restore-math-greek-herk "<#03D6>") => "<varpi>")
(check (restore-math-greek-herk "<#3C6>*<#3D6>*<#3D5>=1")
  =>
  "<varphi>*<varpi>*<phi>=1"
) ;check
(check (restore-math-greek-herk "plain text <#3B1>") => "plain text <#3B1>")

(check-report)
