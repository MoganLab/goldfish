(import (liii check)
        (liii string)
        (liii pi compass render)
) ;import

(check-set-mode! 'report-failed)

;; pi-compass-truncate-codemap
;; 未超限时原样返回。
(check (pi-compass-truncate-codemap "abc\ndef" 100) => "abc\ndef")

;; 超限时保留整行，并追加脚本模式截断提示。
(let* ((line (make-string 80 #\a))
       (markdown (string-join (make-list 10 line) "\n"))
       (result (pi-compass-truncate-codemap markdown 250))
      ) ;
  (check (string-ends? result
           "\n... (codemap truncated, run --codemap to see full output)"
         ) ;string-ends?
    =>
    #t
  ) ;check
  (check (< (string-length result) 400) => #t)
  (check (not (string=? result markdown)) => #t)
) ;let*

(check-report)
