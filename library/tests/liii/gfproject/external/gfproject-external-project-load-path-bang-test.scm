;; gfproject-external-project-load-path!
;; 把外部项目的项目根与 domain 根注入 *load-path*。
;;
;; 语法
;; ----
;; (gfproject-external-project-load-path! tool-root) -> boolean
;;
;; 参数
;; ----
;; tool-root : string
;; 外部工具所在目录，通常是 tools_dir 展开后拼接 tool 得到的路径。
;;
;; 返回值
;; ----
;; boolean
;; 找到外部项目 gfproject.json 并完成注入时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; - 从工具根向上找到最近的 gfproject.json，以该项目根及其中声明的所有
;;   domain root 作为加载根。
;; - 幂等：重复调用不会向 *load-path* 重复追加相同路径。
;; - 找不到 gfproject.json 时返回 #f，由调用方保留工具根。

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

(define (normalized path-str)
  (path->string (path-resolve (path path-str) #f))
) ;define

(let* ((base
         (path->string
           (path-join (path-temp-dir)
             (string-append "gfproject-external-" (number->string (getpid)))
           ) ;path-join
         ) ;path->string
       ) ;base
       (ext (path->string (path-join base "ext")))
       (tool-root (path->string (path-join ext "foo")))
       (plain-root (path->string (path-join base "plain")))
      ) ;
  (mkdir base)
  (mkdir ext)
  (mkdir tool-root)
  ;; domain root 必须是真实目录：macOS 上 /var 是符号链接，
  ;; path-resolve 对不存在的路径不会展开符号链接，会导致与注入值不一致。
  (mkdir (path->string (path-join ext "bar")))
  (mkdir plain-root)

  (write-text (path->string (path-join ext "gfproject.json"))
    (string-append "{\"domains\": {\"foo\": {\"root\": \"foo/\"},"
      " \"bar\": {\"root\": \"bar/\"}}}\n"
    ) ;string-append
  ) ;write-text

  ;; 注入项目根与全部 domain 根
  (check (gfproject-external-project-load-path! tool-root) => #t)
  (check-true (pair? (member (normalized ext) *load-path* string=?)))
  (check-true (pair? (member (normalized tool-root) *load-path* string=?)))
  (check-true
    (pair?
      (member (normalized (path->string (path-join ext "bar"))) *load-path* string=?)
    ) ;pair?
  ) ;check-true

  ;; 幂等：再次调用不改变 *load-path*
  (let ((count (length *load-path*)))
    (gfproject-external-project-load-path! tool-root)
    (check (length *load-path*) => count)
  ) ;let

  ;; 没有 gfproject.json 时返回 #f，不注入
  (let ((count (length *load-path*)))
    (check (gfproject-external-project-load-path! plain-root) => #f)
    (check (length *load-path*) => count)
  ) ;let

  (delete-file (path->string (path-join ext "gfproject.json")))
  (path-rmdir (path tool-root))
  (path-rmdir (path (path->string (path-join ext "bar"))))
  (path-rmdir (path ext))
  (path-rmdir (path plain-root))
  (path-rmdir (path base))
) ;let*

(check-report)
