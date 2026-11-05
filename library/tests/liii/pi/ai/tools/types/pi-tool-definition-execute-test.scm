;; pi-tool-definition-execute
;; 获取工具定义 record 的 execute 字段。
;;
;; 语法
;; ----
;; (pi-tool-definition-execute obj)
;;
;; 参数
;; ----
;; obj : any?
;; 工具定义 record 实例。
;;
;; 返回值
;; ----
;; procedure?
;; execute 字段的值。

(import (liii check) (liii pi ai tools types))

(check-set-mode! 'report-failed)

(define exec-read-a (lambda (args) "ok"))

(define exec-bash-b (lambda (args) "ok"))
(let* ((obj-a (%make-pi-tool-definition "read"
                "读取文件内容"
                '(("type" . "object"))
                exec-read-a
                "read file.txt"
                '("只读文件")
              ) ;%make-pi-tool-definition
       ) ;obj-a
       (obj-b (%make-pi-tool-definition "bash"
                "执行 shell 命令"
                '(("type" . "object"))
                exec-bash-b
                "ls -l"
                '()
              ) ;%make-pi-tool-definition
       ) ;obj-b
      ) ;
  (check-true (eq? (pi-tool-definition-execute obj-a) exec-read-a))
  (check-true (eq? (pi-tool-definition-execute obj-b) exec-bash-b))
) ;let*

(check-report)
