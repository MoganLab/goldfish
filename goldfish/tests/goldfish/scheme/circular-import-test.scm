;; 循环 import 检测测试
;;
;; R7RS library import 形成循环引用时（A 导入 B，B 导入 A），加载器必须
;; 检测到循环并抛出明确的错误，而不是无限递归直到 C 栈溢出（段错误）。
;;
;; 背景：gfcode2 的 (liii pi tui cli) 经由
;;   discovery -> config -> args -> discovery
;; 形成隐式循环，导致 `gf pi` 在深度 import 链中触发约 5 万层 C 栈
;; 嵌套后崩溃。

(import (liii check) (liii path) (liii os) (scheme base))

(check-set-mode! 'report-failed)

;; 两个库互相 import：A -> B -> A 的直接循环
(let* ((probe-root
         (path-join (path-temp-dir)
           (string-append "gf-circ-import-" (number->string (getpid)))
         ) ;path-join
       ) ;probe-root
       (probe-liii (path-join probe-root "liii"))
       (file-a (path-join probe-liii "circ-a.scm"))
       (file-b (path-join probe-liii "circ-b.scm"))
       (old-load-path *load-path*)
      ) ;
  (mkdir (path->string probe-root))
  (mkdir (path->string probe-liii))
  (path-write-text file-a
    (string-append "(define-library (liii circ-a)\n"
      "  (import (liii circ-b))\n" "  (export circ-a-val)\n"
      "  (begin (define circ-a-val 1)))\n"
    ) ;string-append
  ) ;path-write-text
  (path-write-text file-b
    (string-append "(define-library (liii circ-b)\n"
      "  (import (liii circ-a))\n" "  (export circ-b-val)\n"
      "  (begin (define circ-b-val 2)))\n"
    ) ;string-append
  ) ;path-write-text
  (dynamic-wind
    (lambda ()
      (set! *load-path* (append *load-path* (list (path->string probe-root))))
    ) ;lambda
    (lambda ()
      ;; 循环 import 必须报错，而不是段错误
      (check-catch 'error (import (liii circ-a)))
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (path-unlink file-a #t)
      (path-unlink file-b #t)
      (path-rmdir (path->string probe-liii))
      (path-rmdir (path->string probe-root))
    ) ;lambda
  ) ;dynamic-wind
) ;let*

;; 自循环：库直接 import 自己
(let* ((probe-root
         (path-join (path-temp-dir)
           (string-append "gf-self-import-" (number->string (getpid)))
         ) ;path-join
       ) ;probe-root
       (probe-liii (path-join probe-root "liii"))
       (file-s (path-join probe-liii "selfimp.scm"))
       (old-load-path *load-path*)
      ) ;
  (mkdir (path->string probe-root))
  (mkdir (path->string probe-liii))
  (path-write-text file-s
    (string-append "(define-library (liii selfimp)\n"
      "  (import (liii selfimp))\n" "  (export selfimp-val)\n"
      "  (begin (define selfimp-val 3)))\n"
    ) ;string-append
  ) ;path-write-text
  (dynamic-wind
    (lambda ()
      (set! *load-path* (append *load-path* (list (path->string probe-root))))
    ) ;lambda
    (lambda () (check-catch 'error (import (liii selfimp))))
    (lambda ()
      (set! *load-path* old-load-path)
      (path-unlink file-s #t)
      (path-rmdir (path->string probe-liii))
      (path-rmdir (path->string probe-root))
    ) ;lambda
  ) ;dynamic-wind
) ;let*

;; 三库间接循环：A -> B -> C -> A
(let* ((probe-root
         (path-join (path-temp-dir)
           (string-append "gf-tri-import-" (number->string (getpid)))
         ) ;path-join
       ) ;probe-root
       (probe-liii (path-join probe-root "liii"))
       (file-a (path-join probe-liii "tri-a.scm"))
       (file-b (path-join probe-liii "tri-b.scm"))
       (file-c (path-join probe-liii "tri-c.scm"))
       (old-load-path *load-path*)
      ) ;
  (mkdir (path->string probe-root))
  (mkdir (path->string probe-liii))
  (path-write-text file-a
    (string-append "(define-library (liii tri-a)\n" "  (import (liii tri-b))\n"
      "  (export tri-a-val)\n" "  (begin (define tri-a-val 1)))\n"
    ) ;string-append
  ) ;path-write-text
  (path-write-text file-b
    (string-append "(define-library (liii tri-b)\n" "  (import (liii tri-c))\n"
      "  (export tri-b-val)\n" "  (begin (define tri-b-val 2)))\n"
    ) ;string-append
  ) ;path-write-text
  (path-write-text file-c
    (string-append "(define-library (liii tri-c)\n" "  (import (liii tri-a))\n"
      "  (export tri-c-val)\n" "  (begin (define tri-c-val 3)))\n"
    ) ;string-append
  ) ;path-write-text
  (dynamic-wind
    (lambda ()
      (set! *load-path* (append *load-path* (list (path->string probe-root))))
    ) ;lambda
    (lambda () (check-catch 'error (import (liii tri-a))))
    (lambda ()
      (set! *load-path* old-load-path)
      (path-unlink file-a #t)
      (path-unlink file-b #t)
      (path-unlink file-c #t)
      (path-rmdir (path->string probe-liii))
      (path-rmdir (path->string probe-root))
    ) ;lambda
  ) ;dynamic-wind
) ;let*

;; 对照组：无循环的库应正常加载
(let* ((probe-root
         (path-join (path-temp-dir)
           (string-append "gf-nocirc-import-" (number->string (getpid)))
         ) ;path-join
       ) ;probe-root
       (probe-liii (path-join probe-root "liii"))
       (file-a (path-join probe-liii "nocirc-a.scm"))
       (file-b (path-join probe-liii "nocirc-b.scm"))
       (old-load-path *load-path*)
      ) ;
  (mkdir (path->string probe-root))
  (mkdir (path->string probe-liii))
  (path-write-text file-a
    (string-append "(define-library (liii nocirc-a)\n"
      "  (import (liii nocirc-b))\n" "  (export nocirc-a-val)\n"
      "  (begin (define nocirc-a-val (* nocirc-b-val 10))))\n"
    ) ;string-append
  ) ;path-write-text
  (path-write-text file-b
    (string-append "(define-library (liii nocirc-b)\n"
      "  (import (scheme base))\n" "  (export nocirc-b-val)\n"
      "  (begin (define nocirc-b-val 7)))\n"
    ) ;string-append
  ) ;path-write-text
  (dynamic-wind
    (lambda ()
      (set! *load-path* (append *load-path* (list (path->string probe-root))))
    ) ;lambda
    (lambda () (import (liii nocirc-a)) (check nocirc-a-val => 70))
    (lambda ()
      (set! *load-path* old-load-path)
      (path-unlink file-a #t)
      (path-unlink file-b #t)
      (path-rmdir (path->string probe-liii))
      (path-rmdir (path->string probe-root))
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report "\n\nCheck report of circular-import-test.scm => ")
