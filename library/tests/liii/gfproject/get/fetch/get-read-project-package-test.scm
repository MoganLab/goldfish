;; get-read-project-package
;; 读取项目 gfproject.json 并解析 package 元数据。
;;
;; 语法
;; ----
;; (get-read-project-package dir)
;;
;; 参数
;; ----
;; dir : path?
;; 项目根目录。
;;
;; 返回值
;; ----
;; pair?
;; (cons package config)；缺少 package 元数据时抛 value-error。

(import (liii check) (liii os) (liii path) (liii shutil)
  (liii gfproject package) (liii gfproject get fetch)
) ;import

(check-set-mode! 'report-failed)

(define base (path-join (path-temp-dir)
               (string-append "gf-get-pkg-" (number->string (getpid)))
             ) ;path-join
) ;define
(path-mkdir base #t #t)

;; 有 package：返回记录与原始配置
(path-write-text (path-join base "gfproject.json")
  "{\"package\":{\"name\":\"demo\",\"organization\":\"liii\"},\"domains\":{}}"
) ;path-write-text
(let ((result (get-read-project-package base)))
  (check-true (gfproject-package? (car result)))
  (check (gfproject-package-id (car result)) => "demo@liii")
  (check-true (pair? (cdr result)))
) ;let

;; 无 package：报错
(path-write-text (path-join base "gfproject.json") "{\"domains\":{}}")
(check-catch 'value-error (get-read-project-package base))

;; 无 gfproject.json：报错
(path-unlink (path-join base "gfproject.json"))
(check-catch 'value-error (get-read-project-package base))

(rmtree (path->string base) #t)

(check-report)
