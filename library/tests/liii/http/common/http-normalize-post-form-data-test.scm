;; http-normalize-post-form-data
;; 规范化 POST 表单数据，空数据归一为空列表，alist 走字符串规范化。
;;
;; 语法
;; ----
;; (http-normalize-post-form-data who data)
;;
;; 参数
;; ----
;; who  : string?  调用方函数名称，用于错误信息前缀。
;; data : any      表单数据，支持空列表、空字符串或关联列表。
;;
;; 返回值
;; ----
;; alist?
;; 规范化后的表单字段关联列表；空数据返回空列表。
;;
;; 错误处理
;; ----
;; type-error
;; data 既不是空数据也不是关联列表时抛出。

(import (liii check) (liii http common))

(check-set-mode! 'report-failed)

;; 空列表与空字符串归一为空列表
(check (http-normalize-post-form-data "test" '()) => '())
(check (http-normalize-post-form-data "test" "") => '())

;; 关联列表按键值规范化为字符串
(check (http-normalize-post-form-data "test" '(("meta" . "release"))) =>
  '(("meta" . "release"))
) ;check
(check (http-normalize-post-form-data "test" '((count . 3))) => '(("count" . "3")))

;; 非空字符串抛出 type-error
(check-catch 'type-error (http-normalize-post-form-data "test" "raw-body"))

;; 非关联列表抛出 type-error
(check-catch 'type-error (http-normalize-post-form-data "test" 42))
(check-catch 'type-error (http-normalize-post-form-data "test" (list "a" "b")))
(check-catch 'type-error (http-normalize-post-form-data "test" '#(1 2)))

(check-report)
