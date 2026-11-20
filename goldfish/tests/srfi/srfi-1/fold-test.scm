;; fold
;; 从左到右遍历列表并累积结果。
;;
;; 语法
;; ----
;; (fold kons knil clist1 clist2 ...)
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
;; 返回折叠后的最终结果。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (fold + 0 '(1 2 3 4)) => 10)
(check (fold + 0 '()) => 0)

(check-catch 'type-error (fold 0 + '(1 2 3 4)))

(check (fold cons () '(1 2 3 4)) => '(4 3 2 1))

(check
  (fold (lambda (x count) (if (symbol? x) (+ count 1) count)) 0 '(a b 1 2 3 4))
  =>
  2
) ;check

(check (fold + 0 '(1 2 3) '(4 5 6)) => 21)
(check (fold + 0 '(1 2 3 4) '(10 20 30)) => 66)
(check (fold list '() '(1 2 3) '(a b c)) => '(3 c (2 b (1 a ()))))
(check-catch 'type-error (fold 0 + '(1 2 3) 'a))

;; 单列表点列表抛 type-error
(check-catch 'type-error (fold + 0 '(1 2 . 3)))

;; 单列表路径深层折叠不消耗 Scheme 栈
(let ((l (iota 100000)))
  (check (fold + 0 l) => 4999950000)
) ;let

;; GC 压力回归测试：闭包 f + 中间累加值，反复触发 GC
(let ((l (iota 10000)))
  (do ((i 0 (+ i 1)))
    ((= i 200))
    (fold (lambda (x acc) (if (even? x) (+ acc 1) acc)) 0 l)
    (fold cons '() (map (lambda (x) (list x)) '(1 2 3 4 5)))
  ) ;do
  (check
    (fold (lambda (x acc) (if (even? x) (+ acc 1) acc)) 0 l)
    =>
    5000
  ) ;check
) ;let


;; 回归测试：回调内 (catch ...) 抛异常并捕获后，fold 必须继续遍历到列表末尾。
;; 旧实现的 g_fold 用 s7_apply_function 回调，不建立跳转帧；当回调内的 (catch #t ...)
;; 被触发，longjmp 会越过 g_fold 的 C 栈帧，迭代静默中止，fold 不返回（表现为串行
;; 测试挂起）。改用 s7_call 后，catch 的跳转帧落在 g_fold 帧内，迭代可正常完成。
;; 这里偶数元素抛异常、奇数元素透传 acc，期望最终 acc 等于抛异常的次数（3）。
(check
  (fold
    (lambda (x acc)
      (catch #t
        (lambda () (if (even? x) (error 'boom) acc))
        (lambda (tag . info) (+ acc 1))
      ) ;catch
    ) ;lambda
    0
    '(1 2 3 4 5 6)
  ) ;fold
  =>
  3
) ;check

;; 大列表版本：确保遍历完整、不静默中止。
(let ((l (iota 10000)))
  (check
    (fold
      (lambda (x acc)
        (catch #t
          (lambda () (if (zero? (modulo x 7)) (error 'boom) acc))
          (lambda (tag . info) (+ acc 1))
        ) ;catch
      ) ;lambda
      0
      l
    ) ;fold
    =>
    (length
      (filter (lambda (x) (zero? (modulo x 7))) l)
    ) ;length
  ) ;check
) ;let

(check-report)
