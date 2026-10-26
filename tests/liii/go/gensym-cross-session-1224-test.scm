(import (liii check) (liii go))

;; 复现 issue #1224：多 s7 会话并发时 gen 符号被错误回收
;;
;; 根因：s7.c 的 mark_function[] 是进程级全局数组，所有会话共享。
;; mark_function[T_SYMBOL] 随各会话 gensym 表的变化全局抖动：
;;   add_gensym (s7.c)            -> just_mark（任何会话创建 gensym）
;;   sweep / remove_gensym_from_heap
;;     中本会话表空 (loc == 0)    -> mark_noop
;; gensym 是唯一依赖该标记函数的堆上符号；若会话 A 持有可达 gensym
;; 并 GC 时，恰好另一会话把自己的表清到空（boot 加载 scheme 库时
;; define-syntax 密集段：syntax-rules.scm 的 (gensym "trans_") 创建，
;; define 到 rootlet 时 remove），A 的 gensym 不会被标记而被回收，
;; 代码随后读到 <free cell!>（CI 上表现为
;; "bad variable name <free cell!> in let"，进程 exit 1）。
;;
;; 编排（复刻 CI：gf test 并行时 GoThreadPool 首次初始化）：
;;   1. 主会话持有可达 gensym
;;   2. 首次 go 操作触发 N 个 worker 会话并发 boot（boot.scm 加载 =
;;      define-syntax 密集抖动段，反复把全局置 mark_noop）
;;   3. 主会话高频 (gc) 探测 *live-gs* 是否仍为符号
;; 期望：修复前探测到 *live-gs* 变 #f（gen 符号被跨会话回收）；
;;       修复后始终 #t，探测超时返回 'alive-after-timeout。
;; 本地复现可用 GOLDFISH_GO_WORKERS=64 放大并发 boot 窗口。

(define *probe-seconds* 15)

(define *live-gs* (gensym))

(define ch-boot (make-chan 1))

(go-apply/source '(lambda () 'booted) (rootlet) '() ch-boot)

(define *probe-result*
  (let ((start (g_now-ms)))
    (let loop
      ((alive #t))
      (cond ((not alive) 'gensym-recycled)
            ((> (- (g_now-ms) start) (* *probe-seconds* 1000)) 'alive-after-timeout)
            (else (gc) (loop (symbol? *live-gs*)))
      ) ;cond
    ) ;let
  ) ;let
) ;define

(display "Probe result: ")
(display *probe-result*)
(newline)

(check *probe-result* => 'alive-after-timeout)

(check-report)
