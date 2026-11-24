;; get-declared-ref
;; 取 depends 中登记的 ref（仅远程 url 来源）。
;;
;; 语法
;; ----
;; (get-declared-ref id)
;;
;; 参数
;; ----
;; id : string?
;; 包标识 name@organization。
;;
;; 返回值
;; ----
;; string? or #f
;; 登记的 ref；本地路径来源或未登记 ref 时返回 #f。

(import (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii gfproject get store)
) ;import

(check-set-mode! 'report-failed)

(define orig-home (getenv "HOME"))

(define orig-profile (getenv "USERPROFILE"))

(define home-dir
  (path-join (path-temp-dir)
    (string-append "gf-get-ref-" (number->string (getpid)))
  ) ;path-join
) ;define

(define (set-test-home! dir)
  (putenv "HOME" dir)
  (when (os-windows?)
    (putenv "USERPROFILE" dir)
  ) ;when
) ;define

(define (restore-test-home!)
  (if orig-home (putenv "HOME" orig-home) (unsetenv "HOME"))
  (when (os-windows?)
    (if orig-profile (putenv "USERPROFILE" orig-profile) (unsetenv "USERPROFILE"))
  ) ;when
) ;define

(path-mkdir (path-join home-dir ".gf") #t #t)
(path-write-text (path-join home-dir ".gf" "gfproject.json")
  (string-append "{\"depends\":{"
    "\"a@liii\":{\"url\":\"https://example.com/a\",\"ref\":\"v1.0.0\"},"
    "\"b@liii\":{\"url\":\"https://example.com/b\"},"
    "\"c@liii\":{\"path\":\"~/git/c\",\"local\":true}}}"
  ) ;string-append
) ;path-write-text
(set-test-home! (path->string home-dir))

;; 远程包登记了 ref
(check (get-declared-ref "a@liii") => "v1.0.0")
;; 远程包未登记 ref
(check (get-declared-ref "b@liii") => #f)
;; 本地路径来源无 ref
(check (get-declared-ref "c@liii") => #f)
;; 未注册包
(check (get-declared-ref "nosuch@liii") => #f)

(restore-test-home!)
(rmtree (path->string home-dir) #t)

(check-report)
