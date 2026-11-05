;; %make-pi-tool-definition
;; 构造工具定义 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-tool-definition name description parameters execute snippet guidelines)
;;
;; 参数
;; ----
;; name : string?
;; 工具名称
;;
;; description : string?
;; 工具描述
;;
;; parameters : alist?
;; 调用参数的 JSON schema 定义
;;
;; execute : procedure?
;; 执行工具的函数
;;
;; snippet : string?
;; 用法示例片段
;;
;; guidelines : list?
;; 使用指南条目列表
;;
;; 返回值
;; ----
;; pi-tool-definition?
;; 工具定义 record 实例。

(import (liii check) (liii pi ai tools types))

(check-set-mode! 'report-failed)

(define exec-read-a (lambda (args) "ok"))
(let ((obj (%make-pi-tool-definition "read"
             "读取文件内容"
             '(("type" . "object"))
             exec-read-a
             "read file.txt"
             '("只读文件")
           ) ;%make-pi-tool-definition
      ) ;obj
     ) ;
  (check-true (pi-tool-definition? obj))
  (check (pi-tool-definition-name obj) => "read")
  (check (pi-tool-definition-description obj) => "读取文件内容")
  (check (pi-tool-definition-parameters obj) => '(("type" . "object")))
  (check-true (eq? (pi-tool-definition-execute obj) exec-read-a))
  (check (pi-tool-definition-snippet obj) => "read file.txt")
  (check (pi-tool-definition-guidelines obj) => '("只读文件"))
) ;let

(check-report)
