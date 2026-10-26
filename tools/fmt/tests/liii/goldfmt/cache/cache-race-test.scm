;; (liii goldfmt cache) 并发回归测试
;;
;; 背景：Windows CI 上 `gf fmt -e cpp -j 4` 格式化 4 个内容相同的文件时，
;; 曾出现 "Total files formatted: 4, Files updated: 3, Files unchanged: 0"：
;; 内容相同的文件格式化后 SHA-256 一致，对应同一缓存目录；
;; goldtool-cache-touch 的 (unless (path-dir? ...) (mkdir ...)) 存在
;; 检查-创建窗口，并发 worker 同时创建同一目录时，后来者的 mkdir 抛
;; file-exists-error，被上层 run-one-task 的 catch 兜底成 'failed，
;; 该文件的格式化结果从计数中丢失。
;;
;; 本测试用通道屏障同步多个 worker，同时 touch 相同内容文件的缓存，
;; 断言所有 worker 均成功、无异常。每轮使用全新内容，保证目标缓存目录
;; 事先不存在。

(set! *load-path* (cons "tools/fmt" *load-path*))
(set! *load-path* (cons "tools/common" *load-path*))

(import (liii check)
  (liii go)
  (liii os)
  (liii path)
  (liii goldfmt cache)
) ;import

(check-set-mode! 'report-failed)

(define *workers* 8)
(define *rounds* 5)

(define sandbox
  (path->string
    (path-join (path (path-temp-dir))
      (string-append "goldfmt-cache-race-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define (remove-tree target)
  (cond ((path-file? target) (path-unlink target #t))
        ((path-dir? target)
         (vector-for-each remove-tree (path-list-path target))
         (path-rmdir target)
        ) ;
        (else #f)
  ) ;cond
) ;define

;; 一轮竞态测试：*workers* 个文件内容完全相同（盐 = 进程号 + 轮次，
;; 跨进程、跨轮次唯一，确保缓存目录事先不存在），用通道屏障同步后
;; 同时 touch 缓存，返回失败的 worker 数。
(define (cache-race-once r)
  (let* ((salt (string-append (number->string (getpid)) "-" (number->string r)))
         (content (string-append ";; cache-race " salt "\n"))
         (dir (path->string
                (path-join (path sandbox) (string-append "r" (number->string r)))
              ) ;path->string
         ) ;dir
         (gate (make-chan 1))
         (result-ch (make-chan *workers*))
        ) ;
    (mkdir dir)
    (let loop
      ((i 0))
      (when (< i *workers*)
        (let ((f (path->string
                   (path-join (path dir)
                     (string-append "f" (number->string i) ".txt")
                   ) ;path-join
                 ) ;path->string
              ) ;f
            ) ;
          (path-write-text (path f) content)
          ;; worker：等发令信号后与其他 worker 同时进入 fmt-cache-touch
          (go-apply
            (lambda (gate file)
              (chan-recv! gate 60000)
              (fmt-cache-touch file)
              'ok
            ) ;lambda
            (list gate f)
            result-ch
          ) ;go-apply
          (loop (+ i 1))
        ) ;let
      ) ;when
    ) ;let
    ;; 发令：关闭通道，所有阻塞在 gate 上的 worker 同时放行
    (chan-close! gate)
    (let collect
      ((k 0) (fails 0))
      (if (= k *workers*)
        fails
        (let ((res (chan-recv! result-ch 60000)))
          (collect (+ k 1)
            (if (and (pair? res) (eq? (car res) 'ok)) fails (+ fails 1))
          ) ;let
        ) ;let
      ) ;if
    ) ;let
  ) ;let*
) ;define

(dynamic-wind
  (lambda () (remove-tree sandbox) (mkdir sandbox))
  (lambda ()
    (check
      (let round-loop
        ((r 0) (acc 0))
        (if (= r *rounds*)
          acc
          (round-loop (+ r 1) (+ acc (cache-race-once r)))
        ) ;if
      ) ;let
      => 0
    ) ;check
  ) ;lambda
  (lambda () (remove-tree sandbox))
) ;dynamic-wind

(check-report)
