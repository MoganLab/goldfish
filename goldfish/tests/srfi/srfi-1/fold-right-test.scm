;; fold-right
;; 从右向左遍历列表并累积结果。
;;
;; 语法
;; ----
;; (fold-right kons knil clist1 clist2 ...)
;;
;; 参数
;; ----
;; kons : procedure?
;; 折叠函数，接受累积值和列表元素作为参数。
;;
;; knil : any
;; 初始累积值。
;;
;; clist1, clist2, ... : list?
;; 要折叠的列表。
;;
;; 返回值
;; ------
;; any
;; 返回从右向左折叠后的最终结果。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (fold-right + 0 '(1 2 3 4)) => 10)
(check (fold-right + 0 '()) => 0)

(check (fold-right (lambda (x count) (if (symbol? x) (+ count 1) count))
         0
         '(a b 1 2 3 4))
  =>
  2)

(check (fold-right cons () '(1 2 3 4)) => '(1 2 3 4))

(check (fold-right + 0 '(1 2 3) '(4 5 6)) => 21)
(check (fold-right + 0 '(1 2 3 4) '(10 20 30)) => 66)
(check (fold-right list '() '(1 2 3) '(a b c)) => '(1 a (2 b (3 c ()))))
(check-catch 'type-error (fold-right 0 + '(1 2 3) 'a))

;; fold-right 保持右结合顺序
(check (fold-right (lambda (x acc) (string-append x acc)) "" '("a" "b" "c"))
  =>
  "abc")

;; 单列表点列表抛 type-error
(check-catch 'type-error (fold-right + 0 '(1 2 . 3)))

;; 单列表路径深层右折叠不消耗 Scheme 栈
(let ((l (iota 100000)))
  (check (fold-right + 0 l) => 4999950000))

;; GC 压力回归测试：闭包 f + 逆序副本 + 中间累加值，反复触发 GC
(let ((l (iota 10000)))
  (do ((i 0 (+ i 1)))
    ((= i 200))
    (fold-right (lambda (x acc) (if (even? x) (+ acc 1) acc)) 0 l)
    (fold-right cons '() (map (lambda (x) (list x)) '(1 2 3 4 5))))
  (check (fold-right (lambda (x acc) (if (even? x) (+ acc 1) acc)) 0 l) => 5000))


;; 回归测试：回调内 (catch ...) 抛异常并捕获后，fold-right 必须继续遍历到列表末尾。
;; 旧实现的 g_fold_right 用 s7_apply_function 回调，不建立跳转帧；当回调内的
;; (catch #t ...) 被触发，longjmp 会越过 g_fold_right 的 C 栈帧，迭代静默中止，
;; fold-right 不返回。改用 s7_call 后，catch 的跳转帧落在 g_fold_right 帧内，
;; 迭代可正常完成。
(check
  (fold-right
    (lambda (x acc)
      (catch #t
        (lambda () (if (even? x) (error 'boom) acc))
        (lambda (tag . info) (+ acc 1))))
    0
    '(1 2 3 4 5 6))
  => 3)

;; 大列表版本：确保遍历完整、不静默中止。
(let ((l (iota 10000)))
  (check
    (fold-right
      (lambda (x acc)
        (catch #t
          (lambda () (if (zero? (modulo x 7)) (error 'boom) acc))
          (lambda (tag . info) (+ acc 1))))
      0
      l)
    => (length (filter (lambda (x) (zero? (modulo x 7))) l))))

(check-report)
