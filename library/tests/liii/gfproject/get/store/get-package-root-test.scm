;; get-package-root
;; 解析已安装包的项目根：本地取 depends.path，远程取 site-lisp 安装目录。
;;
;; 语法
;; ----
;; (get-package-root id)
;;
;; 参数
;; ----
;; id : string?
;; 包标识 name@organization。
;;
;; 返回值
;; ----
;; path? or #f
;; 项目根；未注册时返回 #f。远程版本目录由 lock 的 version 决定（缺省 HEAD）。

(import (liii check) (liii os) (liii path) (liii shutil)
  (liii gfproject get store)
) ;import

(check-set-mode! 'report-failed)

(define orig-home (getenv "HOME"))
(define orig-profile (getenv "USERPROFILE"))
(define home-dir (path-join (path-temp-dir)
                   (string-append "gf-get-root-" (number->string (getpid)))
                 ) ;path-join
) ;define

(path-mkdir (path-join home-dir ".gf") #t #t)
(path-write-text (path-join home-dir ".gf" "gfproject.json")
  (string-append
    "{\"depends\":{"
    "\"local@liii\":{\"path\":\"~/git/local\",\"local\":true},"
    "\"remote@liii\":{\"url\":\"https://example.com/r\"}}}"
  ) ;string-append
) ;path-write-text
(path-write-text (path-join home-dir ".gf" "gfproject.lock")
  "{\"packages\":{\"remote@liii\":{\"version\":\"v2\",\"commit\":\"abc\"}}}"
) ;path-write-text
(putenv "HOME" (path->string home-dir))
(when (os-windows?) (putenv "USERPROFILE" (path->string home-dir)))

;; 本地：~ 展开
(check-true (path-equals? (get-package-root "local@liii")
              (path-join (path-home) "git" "local")
            ) ;path-equals?
) ;check-true

;; 远程：lock 的 version 决定目录
(check-true (path-equals? (get-package-root "remote@liii")
              (path-join (get-site-lisp-dir) "liii" "remote" "v2")
            ) ;path-equals?
) ;check-true

;; 未注册
(check-false (get-package-root "nosuch@liii"))

(if orig-home (putenv "HOME" orig-home) (unsetenv "HOME"))
(when (os-windows?)
  (if orig-profile (putenv "USERPROFILE" orig-profile) (unsetenv "USERPROFILE"))
) ;when
(rmtree (path->string home-dir) #t)

(check-report)
