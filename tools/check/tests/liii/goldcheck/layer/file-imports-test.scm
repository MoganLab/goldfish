(import (scheme base) (liii check) (liii path) (liii goldcheck layer))

;; file-imports
;; 读取文件 define-library 头部，返回 import 块中的库名列表。
;; 遇到 (begin 行停止；注释行跳过。
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
