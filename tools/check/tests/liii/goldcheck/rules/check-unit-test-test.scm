;; check-unit-test
;; 检查单元测试文件是否符合规范：
;; 1. 文档置于文件最顶部（在 import 之前）
;; 2. 文档首行必须为函数名
;; 3. 必须包含简要描述
;; 4. 必须包含【语法】段落
;; 5. 必须包含【参数】段落
;; 6. 必须包含【返回值】段落
;; 7. 小节标题必须属于白名单，且下方紧跟 '----' 分割线
;; 8. (check-report) 必须在最后一行
;; 9. (check-report) 上方必须保留一行空行
;;
;; 语法
;; ----
;; (check-unit-test file-path)
;;
;; 参数
;; ----
;; file-path : string
;; 待检查的 Scheme 测试文件路径。
;;
;; 返回值
;; ------
;; list?
;; 违规列表，每个元素为 (list 'unit-test file-path line reason-string)。
;; 文件符合规范时返回空列表 '()。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii os)
  (liii string)
  (liii goldcheck rules)
) ;import

(check-set-mode! 'report-failed)

(define (write-test-file name content)
  (let ((tmp-file (path->string (path-join (path-temp-dir) name))))
    (path-write-bytes (path tmp-file) (string->utf8 content))
    tmp-file
  ) ;let
) ;define

;; 1. 标准合规用例：全部规范要素齐全，通过
(let* ((good-content (string-append
                       ";; my-func\n"
                       ";; 这是一个测试函数的简要描述。\n"
                       ";;\n"
                       ";; 语法\n"
                       ";; ----\n"
                       ";; (my-func x)\n"
                       ";;\n"
                       ";; 参数\n"
                       ";; ----\n"
                       ";; x : any\n"
                       ";;\n"
                       ";; 返回值\n"
                       ";; ------\n"
                       ";; boolean?\n"
                       ";;\n"
                       ";; 说明\n"
                       ";; ----\n"
                       ";; 1. 详细说明\n"
                       "\n"
                       "(import (scheme base) (liii check))\n"
                       "\n"
                       "(check-set-mode! 'report-failed)\n"
                       "\n"
                       ";; 基本测试\n"
                       "(check 1 => 1)\n"
                       "\n"
                       "(check-report)\n"))
       (tmp-file (write-test-file "gf-check-unit-test-good.scm" good-content))
      ) ;good-content
  (check (check-unit-test tmp-file) => '())
  (path-unlink tmp-file #t)
) ;let*

;; 2. 违规用例：文档未置顶（import 在前）
(let* ((bad-content (string-append
                      "(import (scheme base) (liii check))\n"
                      ";; my-func\n"
                      ";; 描述\n"
                      ";; 语法\n;; ----\n;; (my-func)\n"
                      ";; 参数\n;; ----\n;; 无\n"
                      ";; 返回值\n;; ------\n;; any\n"
                      "\n"
                      "(check 1 => 1)\n"
                      "\n"
                      "(check-report)\n"))
       (tmp-file (write-test-file "gf-check-unit-test-bad-top.scm" bad-content))
       (viols (check-unit-test tmp-file))
      ) ;bad-content
  (check-true (not (null? viols)))
  (check (car (car viols)) => 'unit-test)
  (check-true (string-contains? (list-ref (car viols) 3) "文档必须置于文件最顶部"))
  (path-unlink tmp-file #t)
) ;let*

;; 3. 违规用例：缺少函数名声明（第一行直接是小节标题）
(let* ((bad-content (string-append
                      ";; 语法\n"
                      ";; ----\n"
                      ";; (my-func)\n"
                      ";; 参数\n;; ----\n;; 无\n"
                      ";; 返回值\n;; ------\n;; any\n"
                      "\n"
                      "(import (liii check))\n"
                      "\n"
                      "(check 1 => 1)\n"
                      "\n"
                      "(check-report)\n"))
       (tmp-file (write-test-file "gf-check-unit-test-no-name.scm" bad-content))
       (viols (check-unit-test tmp-file))
      ) ;bad-content
  (check-true (not (null? viols)))
  (check-true (string-contains? (list-ref (car viols) 3) "缺少函数名声明"))
  (path-unlink tmp-file #t)
) ;let*

;; 4. 违规用例：缺少简要描述
(let* ((bad-content (string-append
                      ";; my-func\n"
                      ";; 语法\n"
                      ";; ----\n"
                      ";; (my-func)\n"
                      ";; 参数\n;; ----\n;; 无\n"
                      ";; 返回值\n;; ------\n;; any\n"
                      "\n"
                      "(import (liii check))\n"
                      "\n"
                      "(check 1 => 1)\n"
                      "\n"
                      "(check-report)\n"))
       (tmp-file (write-test-file "gf-check-unit-test-no-desc.scm" bad-content))
       (viols (check-unit-test tmp-file))
      ) ;bad-content
  (check-true (not (null? viols)))
  (check-true (string-contains? (list-ref (car viols) 3) "缺少函数简要描述"))
  (path-unlink tmp-file #t)
) ;let*

;; 5. 违规用例：缺少【语法】段落
(let* ((bad-content (string-append
                      ";; my-func\n"
                      ";; 简要描述\n"
                      ";; 参数\n;; ----\n;; 无\n"
                      ";; 返回值\n;; ------\n;; any\n"
                      "\n"
                      "(import (liii check))\n"
                      "\n"
                      "(check 1 => 1)\n"
                      "\n"
                      "(check-report)\n"))
       (tmp-file (write-test-file "gf-check-unit-test-no-syntax.scm" bad-content))
       (viols (check-unit-test tmp-file))
      ) ;bad-content
  (check-true (not (null? viols)))
  (check-true (string-contains? (list-ref (car viols) 3) "缺少【语法】段落"))
  (path-unlink tmp-file #t)
) ;let*

;; 6. 违规用例：缺少【参数】段落
(let* ((bad-content (string-append
                      ";; my-func\n"
                      ";; 简要描述\n"
                      ";; 语法\n;; ----\n;; (my-func)\n"
                      ";; 返回值\n;; ------\n;; any\n"
                      "\n"
                      "(import (liii check))\n"
                      "\n"
                      "(check 1 => 1)\n"
                      "\n"
                      "(check-report)\n"))
       (tmp-file (write-test-file "gf-check-unit-test-no-params.scm" bad-content))
       (viols (check-unit-test tmp-file))
      ) ;bad-content
  (check-true (not (null? viols)))
  (check-true (string-contains? (list-ref (car viols) 3) "缺少【参数】段落"))
  (path-unlink tmp-file #t)
) ;let*

;; 7. 违规用例：缺少【返回值】段落
(let* ((bad-content (string-append
                      ";; my-func\n"
                      ";; 简要描述\n"
                      ";; 语法\n;; ----\n;; (my-func)\n"
                      ";; 参数\n;; ----\n;; 无\n"
                      "\n"
                      "(import (liii check))\n"
                      "\n"
                      "(check 1 => 1)\n"
                      "\n"
                      "(check-report)\n"))
       (tmp-file (write-test-file "gf-check-unit-test-no-ret.scm" bad-content))
       (viols (check-unit-test tmp-file))
      ) ;bad-content
  (check-true (not (null? viols)))
  (check-true (string-contains? (list-ref (car viols) 3) "缺少【返回值】段落"))
  (path-unlink tmp-file #t)
) ;let*

;; 8. 违规用例：未知的小节标题
(let* ((bad-content (string-append
                      ";; my-func\n"
                      ";; 简要描述\n"
                      ";; 语法\n;; ----\n;; (my-func)\n"
                      ";; 参数\n;; ----\n;; 无\n"
                      ";; 返回值\n;; ------\n;; any\n"
                      ";; 自定义非法标题\n;; ----\n;; 内容\n"
                      "\n"
                      "(import (liii check))\n"
                      "\n"
                      "(check 1 => 1)\n"
                      "\n"
                      "(check-report)\n"))
       (tmp-file (write-test-file "gf-check-unit-test-unknown-sec.scm" bad-content))
       (viols (check-unit-test tmp-file))
      ) ;bad-content
  (check-true (not (null? viols)))
  (check-true (string-contains? (list-ref (car viols) 3) "未知的小节标题"))
  (path-unlink tmp-file #t)
) ;let*

;; 9. 违规用例：包含【示例】小节（应报错并说明下方测试用例即为示例）
(let* ((bad-content (string-append
                      ";; my-func\n"
                      ";; 简要描述\n"
                      ";; 语法\n;; ----\n;; (my-func)\n"
                      ";; 参数\n;; ----\n;; 无\n"
                      ";; 返回值\n;; ------\n;; any\n"
                      ";; 示例\n;; ----\n;; (my-func) => 1\n"
                      "\n"
                      "(import (liii check))\n"
                      "\n"
                      "(check 1 => 1)\n"
                      "\n"
                      "(check-report)\n"))
       (tmp-file (write-test-file "gf-check-unit-test-has-example.scm" bad-content))
       (viols (check-unit-test tmp-file))
      ) ;bad-content
  (check-true (not (null? viols)))
  (check-true (string-contains? (list-ref (car viols) 3) "不应包含【示例】段落，因为文档下方的单元测试用例即为可执行示例"))
  (path-unlink tmp-file #t)
) ;let*

;; 10. 违规用例：小节标题下方未紧跟 '----' 分割线
(let* ((bad-content (string-append
                      ";; my-func\n"
                      ";; 简要描述\n"
                      ";; 语法\n"
                      ";; (my-func)\n"
                      ";; 参数\n;; ----\n;; 无\n"
                      ";; 返回值\n;; ------\n;; any\n"
                      "\n"
                      "(import (liii check))\n"
                      "\n"
                      "(check 1 => 1)\n"
                      "\n"
                      "(check-report)\n"))
       (tmp-file (write-test-file "gf-check-unit-test-no-divider.scm" bad-content))
       (viols (check-unit-test tmp-file))
      ) ;bad-content
  (check-true (not (null? viols)))
  (check-true (string-contains? (list-ref (car viols) 3) "下方必须紧跟 '----' 分割线"))
  (path-unlink tmp-file #t)
) ;let*

;; 11. 违规用例：(check-report) 不在最后一行（末尾有其他代码）
(let* ((bad-content (string-append
                      ";; my-func\n"
                      ";; 简要描述\n"
                      ";; 语法\n;; ----\n;; (my-func)\n"
                      ";; 参数\n;; ----\n;; 无\n"
                      ";; 返回值\n;; ------\n;; any\n"
                      "\n"
                      "(import (liii check))\n"
                      "\n"
                      "(check-report)\n"
                      "(check 1 => 1)\n"))
       (tmp-file (write-test-file "gf-check-unit-test-bad-report-pos.scm" bad-content))
       (viols (check-unit-test tmp-file))
      ) ;bad-content
  (check-true (not (null? viols)))
  (check-true (string-contains? (list-ref (car viols) 3) "(check-report) 必须位于文件最后一行"))
  (path-unlink tmp-file #t)
) ;let*

;; 12. 违规用例：(check-report) 上方未保留空行
(let* ((bad-content (string-append
                      ";; my-func\n"
                      ";; 简要描述\n"
                      ";; 语法\n;; ----\n;; (my-func)\n"
                      ";; 参数\n;; ----\n;; 无\n"
                      ";; 返回值\n;; ------\n;; any\n"
                      "\n"
                      "(import (liii check))\n"
                      "\n"
                      "(check 1 => 1)\n"
                      "(check-report)\n"))
       (tmp-file (write-test-file "gf-check-unit-test-no-blank-above.scm" bad-content))
       (viols (check-unit-test tmp-file))
      ) ;bad-content
  (check-true (not (null? viols)))
  (check-true (string-contains? (list-ref (car viols) 3) "(check-report) 上方必须保留一行空行"))
  (path-unlink tmp-file #t)
) ;let*

;; 13. 违规用例：直接检查已废弃的单文件测试
(let* ((base-dir (path->string (path-join (path-temp-dir) "gf-check-test-legacy2")))
       (sub-dir (path->string (path-join base-dir "foo")))
       (_ (path-mkdir (path sub-dir) :parents #t :exist-ok #t))
       (legacy-file (path->string (path-join base-dir "foo-test.scm")))
       (_ (path-write-bytes (path legacy-file) (string->utf8 ";; legacy\n")))
       (viols (check-unit-test legacy-file))
      ) ;let*
  (check-true (not (null? viols)))
  (check (car (car viols)) => 'unit-test)
  (check-true (string-contains? (list-ref (car viols) 3) "这个方案已经废弃"))
  (path-unlink legacy-file #t)
  (path-rmdir sub-dir)
  (path-rmdir base-dir)
) ;let*

(check-report)
