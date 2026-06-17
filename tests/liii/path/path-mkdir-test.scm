(import (liii check) (liii path) (liii os) (liii string))

(check-set-mode! 'report-failed)

;; path-mkdir
;; 创建目录(对齐 pathlib.Path.mkdir)。
;;
;; 语法
;; ----
;; (path-mkdir path-value parents: bool exist-ok: bool) 或 (path-mkdir p)
;;
;; 返回值
;; ------
;; 创建成功返回 #t。exist-ok=#f 且已存在时报错;parents=#t 递归创建中间目录。

(let ((base (path-join (path-temp-dir)
              (string-append "gf-mkdir-test-" (number->string (getpid)))
            ) ;path-join
      ) ;base
     ) ;
  ;; 清理:确保起始干净
  (when (path-exists? base)
    (rmdir-rec base)
  ) ;when
  (define (rmdir-rec d)
    (when (path-dir? d)
      (for-each (lambda (e)
                  (let ((full (path-join d e)))
                    (if (path-dir? full) (rmdir-rec full) (remove full))
                  ) ;let
                ) ;lambda
        (vector->list (path-list d))
      ) ;for-each
      (rmdir d)
    ) ;when
  ) ;define

  ;; 1. 单级创建
  (path-mkdir base)
  (check-true (path-dir? base))

  ;; 2. exist-ok=#f 默认,已存在时报错
  (check (guard (ex (else 'caught)) (path-mkdir base)) => 'caught)
  ;; exist-ok=#t 已存在不报错
  (check (path-mkdir base exist-ok: #t) => #t)

  ;; 3. parents=#t 递归创建多级
  (let ((deep (path-join base "a" "b" "c")))
    (path-mkdir deep parents: #t)
    (check-true (path-dir? deep))
  ) ;let

  ;; 4. parents=#f 缺中间目录时报错
  (let ((missing (path-join base "nope" "deep")))
    (check (guard (ex (else 'caught)) (path-mkdir missing)) => 'caught)
  ) ;let

  ;; 清理
  (rmdir-rec base)
) ;let

(check-report)
