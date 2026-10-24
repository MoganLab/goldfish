(import (scheme base) (liii check) (liii path) (liii goldcheck layer))

;; file-imports
;; 读取文件顶层形式，返回 import 的库名列表。
;; define-library 文件提取声明子句；裸顶层 (import ...) 文件
;; （扩展插件式）同样提取；注释自动跳过。
;;
;; 语法
;; ----
;; (file-imports path)
;;
;; 参数
;; ----
;; path : string
;; .scm 文件路径。
;;
;; 返回值
;; ------
;; list of string
;; 库名列表（含 scheme 基座，段间空格连接）。

(check-set-mode! 'report-failed)

(let* ((tmp
        (path->string
          (path-join (path-temp-dir) "gf-layer-file-imports-test.scm")))
      )
  (call-with-port (open-output-file tmp)
    (lambda (p)
      (display
        ";; fake (liii comment)\n(define-library (x)\n  (import (scheme base) (liii json)\n    (liii string)\n  ) ;import\n  (begin\n    (define y 1)\n  ) ;begin\n) ;define-library\n"
        p
      ) ;display
    ) ;lambda
  ) ;call-with-port
  (let ((libs (file-imports tmp)))
    ;; 1. 提取 import 块中的库名（同行多库与多行均支持）
    (check-true (member "scheme base" libs))
    (check-true (member "liii json" libs))
    (check-true (member "liii string" libs))
    ;; 2. 注释行中的库名不被提取
    (check-false (member "liii comment" libs))
    ;; 3. (begin 之后的内容不扫描
    (check-false (member "liii after-begin" libs))
    (check (length libs) => 3)
  ) ;let
  (path-unlink (path tmp) #t)
) ;let*

(let* ((tmp
        (path->string
          (path-join (path-temp-dir) "gf-layer-file-imports-bare-test.scm")))
      )
  (call-with-port (open-output-file tmp)
    (lambda (p)
      (display
        ";; bare extension-style file\n(import (scheme base) (liii pi review)\n  (liii pi review modes)\n) ;import\n\n(define (ext api) 'ok)\n"
        p
      ) ;display
    ) ;lambda
  ) ;call-with-port
  (let ((libs (file-imports tmp)))
    ;; 4. 裸顶层 (import ...)（无 define-library）同样提取
    (check-true (member "scheme base" libs))
    (check-true (member "liii pi review" libs))
    (check-true (member "liii pi review modes" libs))
    ;; 5. import 之后的形式不产生库名
    (check (length libs) => 3)
  ) ;let
  (path-unlink (path tmp) #t)
) ;let*

(let* ((tmp
        (path->string
          (path-join (path-temp-dir) "gf-layer-file-imports-mixed-test.scm")))
      )
  (call-with-port (open-output-file tmp)
    (lambda (p)
      (display
        "(define (%helper x) x)\n(set! *opt* 1)\n(define-library (liii demo)\n  (import (scheme base) (liii json))\n  (begin (define y 1))\n) ;define-library\n"
        p
      ) ;display
    ) ;lambda
  ) ;call-with-port
  (let ((libs (file-imports tmp)))
    ;; 6. define-library 非首个顶层形式时仍能提取
    (check-true (member "scheme base" libs))
    (check-true (member "liii json" libs))
    (check (length libs) => 2)
  ) ;let
  (path-unlink (path tmp) #t)
) ;let*
