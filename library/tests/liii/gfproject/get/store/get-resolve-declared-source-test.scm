;; get-resolve-declared-source
;; 解析已注册短包名的来源：用户 depends 中的 url 或本地 path。
;;
;; 语法
;; ----
;; (get-resolve-declared-source id)
;;
;; 参数
;; ----
;; id : string?
;; 包标识 name@organization。
;;
;; 返回值
;; ----
;; pair?
;; (cons kind source)，kind 为 'url 或 'path。
;;
;; 错误处理
;; ----
;; 未注册时抛 value-error，提示首次安装需 --from 或完整 URL/路径。

(import (liii check) (liii os) (liii path) (liii shutil)
  (liii gfproject get store)
) ;import

(check-set-mode! 'report-failed)

(define orig-home (getenv "HOME"))
(define orig-profile (getenv "USERPROFILE"))
(define home-dir (path-join (path-temp-dir)
                   (string-append "gf-get-resolve-" (number->string (getpid)))
                 ) ;path-join
) ;define

(path-mkdir (path-join home-dir ".gf") #t #t)
(path-write-text (path-join home-dir ".gf" "gfproject.json")
  (string-append
    "{\"depends\":{"
    "\"a@liii\":{\"url\":\"https://example.com/a\"},"
    "\"b@liii\":{\"path\":\"~/git/b\",\"local\":true}}}"
  ) ;string-append
) ;path-write-text
(putenv "HOME" (path->string home-dir))
(when (os-windows?) (putenv "USERPROFILE" (path->string home-dir)))

(check (get-resolve-declared-source "a@liii") => '(url . "https://example.com/a"))
(check (get-resolve-declared-source "b@liii") => '(path . "~/git/b"))
(check-catch 'value-error (get-resolve-declared-source "nosuch@liii"))

(if orig-home (putenv "HOME" orig-home) (unsetenv "HOME"))
(when (os-windows?)
  (if orig-profile (putenv "USERPROFILE" orig-profile) (unsetenv "USERPROFILE"))
) ;when
(rmtree (path->string home-dir) #t)

(check-report)
