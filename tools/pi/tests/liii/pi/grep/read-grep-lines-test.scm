(import (liii check)
  (liii path)
  (liii pi grep)
) ;import

(check-set-mode! 'report-failed)

;; %read-grep-lines：读取候选文件并按行切分，读不出来时返回 #f

(let* ((test-base (path-join (path-temp-dir) "pi-test-grep-read"))
       (test-f (path-join test-base "a.txt"))
      ) ;
  (path-mkdir test-base :parents #t :exist-ok #t)
  (path-write-text test-f "line1\nline2\n")
  ;; 1. 正常文件按行返回
  (let ((lines (%read-grep-lines (path test-f))))
    (check-true (pair? lines))
    (check (car lines) => "line1")
    (check (cadr lines) => "line2")
  ) ;let
  ;; 2. 读不出来的文件返回 #f，而不是当成空文件（Issue #1189）
  ;;    这里用目录触发：path-read-lines 对目录抛 file-error
  (check-false (%read-grep-lines (path test-base)))
  (delete-file (path->string test-f))
  (path-rmdir test-base)
) ;let*

(check-report)
