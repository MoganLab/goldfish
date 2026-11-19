;; gfproject-find-project-root
;; 从目录向上定位最近的 gfproject.json 所在项目根。
;;
;; 语法
;; ----
;; (gfproject-find-project-root dir) -> string | #f
;;
;; 参数
;; ----
;; dir : string
;; 起始目录。
;;
;; 返回值
;; ----
;; string 或 #f
;; 找到时返回项目根目录；向上到达文件系统根仍未找到时返回 #f。
;;
;; 说明
;; ----
;; 与 (liii goldtest-project) 的 %find-project-root 语义一致：
;; 只按 gfproject.json 判断项目根，不读取用户层 ~/.gf/gfproject.json。

(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii gfproject external)
) ;import

(check-set-mode! 'report-failed)

(define (write-text path-str text)
  (path-write-text (path path-str) text)
) ;define

(let* ((base
         (path->string
           (path-join (path-temp-dir)
             (string-append "gfproject-find-root-" (number->string (getpid)))
           ) ;path-join
         ) ;path->string
       ) ;base
       (nested (path->string (path-join base "a" "b")))
      ) ;
  (mkdir base)
  (mkdir (path->string (path-join base "a")))
  (mkdir nested)

  ;; 尚无 gfproject.json
  (check (gfproject-find-project-root nested) => #f)

  ;; 上层目录的 gfproject.json 成为项目根
  (write-text (path->string (path-join base "gfproject.json")) "{}\n")
  (check (gfproject-find-project-root nested) => base)
  (check (gfproject-find-project-root base) => base)

  (delete-file (path->string (path-join base "gfproject.json")))
  (path-rmdir (path nested))
  (path-rmdir (path (path->string (path-join base "a"))))
  (path-rmdir (path base))
) ;let*

(check-report)
