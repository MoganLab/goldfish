;; display-type-hint-status
;; 命令行状态行入口：按目标可达 hint 显示，无目标时回退全仓总览。
;;
;; 语法
;; ----
;; (display-type-hint-status result files)
;;
;; 参数
;; ----
;; result : list/vector  规则检查结果，第 4 项为违规集合
;; files  : list of string  检查目标文件列表
;;
;; 返回值
;; ------
;; unspecified
;; 状态行输出到当前输出端口。

(import (scheme base)
  (scheme write)
  (liii check)
  (liii string)
  (liii goldcheck type-hint)
)

(check-set-mode! 'report-failed)

(define (target-file rel)
  (cond ((file-exists? rel) rel)
        ((file-exists? (string-append "tools/check/" rel)) (string-append "tools/check/" rel))
        (else rel)
  ) ;cond
) ;define

;; 1. 空目标回退全仓总览：验证不抛异常（输出依赖仓库根定位）
(let ((out
        (with-output-to-string
          (lambda () (display-type-hint-status (list "type-hint" #t 0.1 (vector)) '()))
        ) ;with-output-to-string
      ) ;out
    )
  (check-true (string? out))
) ;let

;; 2. 单文件目标：只输出可达 hint 的状态行（远少于全仓总览）
(let ((out
        (with-output-to-string
          (lambda ()
            (display-type-hint-status
              (list "type-hint" #t 0.1 (vector))
              (list (target-file "liii/goldcheck/path.scm"))
            ) ;display-type-hint-status
          ) ;lambda
        ) ;with-output-to-string
      ) ;out
    )
  (check-true (string-contains? out "(liii goldcheck path)"))
  (check-true (string-contains? out "(scheme base)"))
) ;let

(check-report)
