;; gfproject-find-user-path
;; 查找用户全局配置 ~/.gf/gfproject.json 的路径。
;;
;; 语法
;; ----
;; (gfproject-find-user-path)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string 或 #f
;; 用户配置文件存在时返回路径；不存在时返回 #f。
;;
;; 说明
;; ----
;; 用户配置与项目配置严格区分：该函数不读取也不影响 g_project-root。

(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii gfproject config)
) ;import

(check-set-mode! 'report-failed)

(define orig-home (getenv "HOME"))
(define orig-profile (getenv "USERPROFILE"))

;; Windows 上 path-home 优先读取 USERPROFILE，POSIX 读取 HOME。
;; 两个变量都指向隔离目录，保证跨平台一致。
(define (set-test-home! dir)
  (putenv "HOME" dir)
  (when (os-windows?) (putenv "USERPROFILE" dir))
) ;define

(define (restore-test-home!)
  (if orig-home (putenv "HOME" orig-home) (unsetenv "HOME"))
  (when (os-windows?)
    (if orig-profile
      (putenv "USERPROFILE" orig-profile)
      (unsetenv "USERPROFILE")
    ) ;if
  ) ;when
) ;define

(let* ((tmp-home
         (path->string
           (path-join (path-temp-dir)
             (string-append "gfproject-user-none-" (number->string (getpid)))
           ) ;path-join
         ) ;path->string
        ) ;tmp-home
       (gf-dir (path->string (path-join tmp-home ".gf")))
       (config-path (path->string (path-join gf-dir "gfproject.json")))
      ) ;
  (mkdir tmp-home)
  (mkdir gf-dir)
  (set-test-home! tmp-home)

  ;; 1. 用户配置不存在时返回 #f
  (check (gfproject-find-user-path) => #f)

  ;; 2. 文件存在时返回 ~/.gf/gfproject.json
  (path-write-text (path config-path) "{}\n")
  (check (gfproject-find-user-path) => config-path)
  (check-true (string-ends? (gfproject-find-user-path) "gfproject.json"))

  (path-unlink (path config-path) #t)
  (path-rmdir (path gf-dir))
  (path-rmdir (path tmp-home))
) ;let*

(restore-test-home!)

(check-report)
