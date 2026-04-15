;; 添加 tools/goldsource 到 load path，以便导入 (liii goldsource)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path*
  (cons "tools/goldsource" *load-path*)
) ;set!

(import (liii check)
  (liii goldsource)
  (liii path)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

;; source-library-path
;; 根据库查询字符串，结合当前 *load-path* 定位源代码文件路径。

(let ((load-root (find-visible-library-root "liii/string"
                 ) ;find-visible-library-root
      ) ;load-root
     ) ;
  (check-true (string? load-root))
  (check-true (path-file? (path-join load-root
                            "liii"
                            "string.scm"
                          ) ;path-join
              ) ;path-file?
  ) ;check-true
) ;let

(let ((source-path (source-library-path "liii/string")
      ) ;source-path
     ) ;
  (check-true (string? source-path))
  (check-true (path-file? source-path))
  (check (path-name source-path)
    =>
    "string.scm"
  ) ;check
) ;let

(check (source-library-path "liii/not-a-real-library"
       ) ;source-library-path
  =>
  #f
) ;check
(check (source-library-path "string-split")
  =>
  #f
) ;check

(check-report)
