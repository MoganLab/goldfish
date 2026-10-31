(import (liii check) (liii go) (liii json) (liii psutil) (liii time))

(check-set-mode! 'report-failed)

;; 复现 issue #1226：JSON true/false/null 符号跨 s7 会话混用
;;
;; 根因：src/liii_json.cpp 的 symbol_true/false/null（及 cached_gc、
;; cached_vector_ref、cached_list_to_vector、cached_string_to_number）
;; 原为进程级静态缓存，(liii go) 的每个 worker 线程 s7_init() 后各自
;; 执行 glue 会互相覆盖：worker 会话里 string->json 解析出的布尔/空值
;; 符号可能属于别的实例，而 s7 的 equal? 对符号按对象标识比较 → 恒为假。
;; 表现：TUI worker 会话中 multikey.json 的 enabled=false 过滤失效
;; （被禁用的 key 仍被使用）、g_json_ref 的 'true/'false -> #t/#f 转换失效。
;; 修复：缓存按 s7_scheme* 实例存放（per-scheme glue 缓存表）。
;;
;; 编排：n 个任务并发驻留（chan-recv! bar 阻塞）促使线程池扩容出多个
;; worker 会话（各自 glue 覆盖静态符号），放行后在各自会话内解析 JSON
;; 并比较符号与 json-ref 布尔转换，结果经任务通道回传主线程。
;; 修复前：除最后 glue 的会话外结果为 #f；修复后全部 #t。
;; 单核机器上线程池只有一个 worker（其自身即最后 glue 者），测试退化
;; 为恒真，多核 CI 上有效。

;; 1. 主会话基线：符号比较与 json-ref 布尔转换正常
(check
  (equal? (cdr (assoc "f" (g_string->json "{\"f\":false}"))) 'false)
  =>
  #t
) ;check
(check (g_json_ref (g_string->json "{\"a\":true}") "a") => #t)

;; 2. worker 会话：解析出的符号必须属于本会话（equal? 与 json-ref 可用）

(define *n* (max 2 (min 4 (go-worker-count))))

(define bar-ch (make-chan 4))

(define res-ch (make-chan 4))

(define *worker-task*
  '(lambda (bar)
     ;; 先驻留：并发任务促使线程池扩容出多个 worker 会话（各自 glue）
     (chan-recv! bar 60000)
     ;; 会话内解析并比较：底层 g_ 胶水函数无需 import
     (let* ((v (g_string->json "{\"t\":true,\"f\":false,\"n\":null}"))
            (t-ok (equal? (cdr (assoc "t" v)) 'true))
            (f-ok (equal? (cdr (assoc "f" v)) 'false))
            (n-ok (equal? (cdr (assoc "n" v)) 'null))
            (r-ok (eq? (g_json_ref (g_string->json "{\"a\":true}") "a") #t)))
       (and t-ok f-ok n-ok r-ok)))
) ;define

(let loop
  ((i 0))
  (when (< i *n*)
    (go-apply/source *worker-task* (rootlet) (list bar-ch) res-ch)
    (loop (+ i 1))
  ) ;when
) ;let

;; 等待线程池扩容：所有任务驻留在各自 worker 会话后再放行
(sleep 1)

(let loop
  ((i 0))
  (when (< i *n*)
    (chan-send! bar-ch 'go 5000)
    (loop (+ i 1))
  ) ;when
) ;let

(let loop
  ((i 0) (all-ok #t))
  (if (= i *n*)
    (check all-ok => #t)
    (let ((r (chan-recv! res-ch 30000)))
      (check (car r) => 'ok)
      (check (eq? (cadr r) #t) => #t)
      (loop (+ i 1) (and all-ok (eq? (cadr r) #t)))
    ) ;let
  ) ;if
) ;let

;; 3. worker 会话大输入循环解析的 RSS 增量（[1311] 周期 GC 在多实例下同样生效）：
;; json_maybe_gc 的字节计数器按实例存放，worker 会话里解析同样周期触发完整 GC，
;; 此场景守住多实例下的内存峰值不随循环次数线性增长

(define (rss)
  (memory-info-resident-set-memory-size (process-memory-info))
) ;define

(define *mem-task*
  '(lambda (bar)
     (chan-recv! bar 60000)
     ;; 在 worker 会话内构造 ~4KB 数字数组并循环解析 2000 次（累计 ~8MB 输入），
     ;; 触发 [1311] 的周期 GC 路径；数字数组无引号转义，便于源码经通道传输
     (let ((parts (list "[")))
       (do ((i 0 (+ i 1)))
         ((= i 999))
         (set! parts
           (cons (number->string i) (if (zero? i) parts (cons "," parts)))))
       (set! parts (cons "]" parts))
       (let ((text (apply string-append (reverse parts))))
         (do ((i 0 (+ i 1))) ((= i 2000)) (g_string->json text))
         'done)))
) ;define

(gc)

(define *mem-r0* (rss))

(define mem-ch (make-chan 1))
(go-apply/source *mem-task* (rootlet) (list bar-ch) mem-ch)
(chan-send! bar-ch 'go 5000)
(check (cadr (chan-recv! mem-ch 120000)) => 'done)
(gc)

(define *mem-delta* (- (rss) *mem-r0*))
(display "rss-delta worker-session(3KB x2000): ")
(display *mem-delta*)
(newline)
(check (< *mem-delta* (* 32 1024 1024)) => #t)

(check-report)
