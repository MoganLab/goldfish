;; pi-config-model-detail-rows
;; 生成模型详情行的标签与取值列表。
;;
;; 语法
;; ----
;; (pi-config-model-detail-rows m p)
;;
;; 参数
;; ----
;; m : alist
;; 模型条目。
;;
;; p : alist
;; 模型所属的 provider 条目。
;;
;; 返回值
;; ----
;; list
;; 标签与取值成对的详情行列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii path)
  (liii pi code modes config state)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(define DIR (string-append FIXTURE "/config-tui"))

(define st (pi-config-state DIR :env '()))

(define (prov id)
  (pi-config-provider st id)
) ;define

(let* ((m (car (pi-config-provider-models (prov "kimi-coding"))))
       (rows (pi-config-model-detail-rows m (prov "kimi-coding")))
       (flat (map (lambda (row) (car row)) rows))
      ) ;
  (check (and (member "id" flat) #t) => #t)
  (check (and (member "api" flat) #t) => #t)
  (check (and (member "key" flat) #t) => #t)
  ;; 详情里不能出现 key 原文
  (check
    (any (lambda (row) (string=? (cadr row) "sk-kimi-test")) rows)
    =>
    #f
  ) ;check
) ;let*

(check-report)
