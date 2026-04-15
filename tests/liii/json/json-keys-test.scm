(import (liii check)
  (liii json)
  (liii base)
  (liii error)
) ;import


(check-set-mode! 'report-failed)


;; json-keys
;; 获取 JSON 对象当前层级的所有键。
;;
;; 语法
;; ----
;; (json-keys json)
;;
;; 参数
;; ----
;; json : any?
;; 要读取键名的 JSON 值。
;;
;; 返回值
;; ----
;; list
;; 当前对象层级的键名列表；非对象时返回空列表。
;;
;; 注意
;; ----
;; 空对象 `'(())` 会返回空列表。
;;
;; 示例
;; ----
;; (json-keys '((bob . ((age . 18) (sex . male))))) => '(bob)
;;
;; 错误处理
;; ----
;; 无。


(let ((j '((bob (age . 18) (sex . male)))))
  (check (json-keys j) => '(bob))
  (check (json-keys (json-ref j 'bob))
    =>
    '(age sex)
  ) ;check
) ;let


(check (json-keys 'null) => '())
(check (json-keys 'true) => '())
(check (json-keys 'false) => '())
(check (json-keys (string->json "[1,2,3]"))
  =>
  '()
) ;check
(check (json-keys (string->json "{}"))
  =>
  '()
) ;check


(check-report)
