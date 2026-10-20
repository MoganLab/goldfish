;; [039] 验收脚本的辅助定义（供 tools/pi/devel/039.scm 通过 (load ...) 使用）
;; 内容分两部分：
;;   1) 命令执行/捕获/清理工具（外部命令一律 :stdin 'null + :timeout 包裹，
;;      禁止静默挂起）；
;;   2) 「迁移前旧实现」参考函数——直接使用 (liii time) 时间 API 复刻 039
;;      之前的代码路径，供验收脚本做新旧行为等价性比对。
;;
;; 说明：本文件是验收基准，不是产品代码；「除 sleep 外禁用 (liii time)」的
;; 约束只针对 tools/pi/liii/ 下的实现文件。

(define %esc (string (integer->char 27)))

(define %tmp-files '())

(define (%strip-ansi s)
  (let loop
    ((codes '("[31m" "[32m" "[33m" "[39m" "[1m" "[0m")) (out s))
    (if (null? codes)
      out
      (loop (cdr codes) (string-replace out (string-append %esc (car codes)) ""))
    ) ;if
  ) ;let
) ;define

(define (%lines s)
  (filter (lambda (l) (not (string=? l ""))) (string-split s "\n"))
) ;define

;; 子命令统一放进子 shell，避免命令内的 || 抢走重定向

(define (%capture tag cmd timeout)
  (let* ((tmp (string-append "tmp-039-" tag ".txt")))
    (set! %tmp-files (cons tmp %tmp-files))
    (let* ((ret (run (list (pi-find-bash-path) "-c" (string-append "(" cmd ") > " tmp " 2>&1"))
                  :stdin   'null
                  :timeout timeout
                ) ;run
           ) ;ret
           (out
             (if (and (file-exists? tmp) (path-file? (path tmp)))
               (%strip-ansi (path-read-text tmp))
               ""
             ) ;if
           ) ;out
          ) ;
      (cons ret out)
    ) ;let*
  ) ;let*
) ;define

(define (%ok? tag cmd timeout)
  (let* ((res (%capture tag cmd timeout)) (rc (car res)))
    (unless (zero? rc)
      (display (string-append "!!! " tag " exited " (number->string rc) "\n" (cdr res))
      ) ;display
    ) ;unless
    rc
  ) ;let*
) ;define

(define (%cmd-lines tag cmd timeout)
  (%lines (cdr (%capture tag cmd timeout)))
) ;define

(define (%cleanup!)
  (for-each (lambda (f) (when (file-exists? f) (delete-file f))) %tmp-files)
  (set! %tmp-files '())
) ;define

(define (%has-substr? lines sub)
  (cond ((null? lines) #f)
        ((string-contains? (car lines) sub) #t)
        (else (%has-substr? (cdr lines) sub))
  ) ;cond
) ;define

(define (%grep-count pattern path)
  (length (%cmd-lines "grep" (string-append "grep -n \"" pattern "\" " path) 60))
) ;define

;; ------------------------------------------------------------------
;; 迁移前旧实现参考函数（复刻 039 之前的 (liii time) 代码路径）
;; ------------------------------------------------------------------

(define (%pad2 n)
  (if (< n 10) (string-append "0" (number->string n)) (number->string n))
) ;define

(define (%pad3 n)
  (if (>= n 100)
    (number->string n)
    (if (>= n 10)
      (string-append "0" (number->string n))
      (string-append "00" (number->string n))
    ) ;if
  ) ;if
) ;define

;; 旧版 pi-iso-8601-utc-now：current-time + time-utc->date + time-nanosecond

(define (%legacy-iso-utc-now)
  (let* ((now (current-time)) (d (time-utc->date now 0)))
    (string-append (number->string (date-year d))
      "-"
      (%pad2 (date-month d))
      "-"
      (%pad2 (date-day d))
      "T"
      (%pad2 (date-hour d))
      ":"
      (%pad2 (date-minute d))
      ":"
      (%pad2 (date-second d))
      "."
      (%pad3 (quotient (time-nanosecond now) 1000000))
      "Z"
    ) ;string-append
  ) ;let*
) ;define

;; 旧版 pi-current-time-millis：time-second * 1000 + time-nanosecond / 1e6

(define (%legacy-millis)
  (let ((now (current-time)))
    (+ (* (time-second now) 1000) (quotient (time-nanosecond now) 1000000))
  ) ;let
) ;define

;; 旧版整秒 epoch（agent 的 current-second / stream 的 time-second）

(define (%legacy-seconds)
  (time-second (current-time))
) ;define
