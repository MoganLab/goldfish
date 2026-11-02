;; report-and-exit
;; 统计所有规则执行结果，输出报告行并退出进程。
;;
;; 语法
;; ----
;; (report-and-exit violations total-elapsed . maybe-cap)
;;
;; 参数
;; ----
;; violations : vector        全部规则汇总的违规向量
;; total-elapsed : number     总运行时间（秒）
;; maybe-cap : integer 或 空  可选的违规展示上限封顶数值
;;
;; 返回值
;; ------
;; 不返回
;; 输出汇总行并调用 exit 退出程序。

(import (scheme base) (scheme write) (liii check) (liii goldcheck report))

(check-set-mode! 'report-failed)

;; 1. 验证为过程对象
(check-true (procedure? report-and-exit))

;; 2. 无违规时以 0 退出
(let* ((exit-code #f) (saved-exit exit))
  (set! exit (lambda (code) (set! exit-code code)))
  (let ((out (with-output-to-string (lambda () (report-and-exit '#() 0.123)))))
    (set! exit saved-exit)
    (check exit-code => 0)
  ) ;let
) ;let*

;; 3. 有违规时以 1 退出
(let* ((exit-code #f) (saved-exit exit))
  (set! exit (lambda (code) (set! exit-code code)))
  (let ((out (with-output-to-string (lambda () (report-and-exit '#((no-crlf "a.scm"
                                                                     1)
                                                                 ) ;# 0.123 5))
             ) ;with-output-to-string
        ) ;out
       ) ;
    (set! exit saved-exit)
    (check exit-code => 1)
  ) ;let
) ;let*

(check-report)
