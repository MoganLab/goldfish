(import (liii check) (goldfish) (scheme eval))

;; 内部 surface 审计：install.scm 的 %internal-names 名单里每个名字都必须
;; 能从 (import (goldfish)) 的程序里求值（解析到绑定）。名单是单一数据源
;; （读 inlet，不在测试里复刻），增删只改 install.scm 一处。
;;
;; boot 期断言不可行（冷/暖加载放置不同，见 install.scm 注释），放到
;; post-boot 的测试里做：这里的求值走与用户程序完全相同的解析路径。

(define surface-names
  (module-ref the-expander-library '%internal-names))

(check-true (pair? surface-names))

(define probe-env (environment '(goldfish)))

(define failures
  (let loop ((names surface-names) (bad '()))
    (if (null? names)
        (reverse bad)
        (loop (cdr names)
              (if (catch #t
                    (lambda () (eval (car names) probe-env) #f)
                    (lambda (type info) #t))
                  (cons (car names) bad)
                  bad)))))

(check failures => '())

(check-report)
