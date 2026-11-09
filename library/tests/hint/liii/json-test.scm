;; hint-liii/json
;; 查询 (liii json) 导出过程的类型签名。
;;
;; 语法
;; ----
;; (hint-liii/json name)
;;
;; 参数
;; ----
;; name : symbol?
;; 待查询的过程名。
;;
;; 返回值
;; ------
;; list of signature | #f
;; 过程的签名列表，形如 (参数类型... -> 返回类型)；
;; 过程未登记签名时返回 #f。
;;
;; 注意
;; ----
;; (hint liii json) 只提供类型签名数据，不导出 (liii json) 的任何过程，
;; 也不影响 (liii json) 的运行行为。

(import (scheme base) (liii check) (liii list) (hint liii json))

(check-set-mode! 'report-failed)

;; (liii json) 的全部导出过程都必须登记 hint 签名

(define %json-exports
  '(;; 序列化与解析
    json-string-escape
    string->json
    json->string
    read-json
    write-json
    ;; 路径读写
    json-ref
    json-set
    json-push
    json-drop
    json-reduce
    ;; 类型谓词
    json-null?
    json-object?
    json-array?
    json-string?
    json-float?
    json-number?
    json-integer?
    json-boolean?
    ;; 状态检查与安全获取器
    json-contains-key?
    json-get-or-else
    json-ref-string
    json-ref-number
    json-ref-integer
    json-ref-boolean
    ;; 辅助工具
    json-keys)
) ;define

(for-each
  (lambda (name) (check-true (pair? (hint-liii/json name))))
  %json-exports
) ;for-each

;; 未登记的过程返回 #f
(check (hint-liii/json 'not-a-json-procedure) => #f)

;; 签名的具体形态
(check (hint-liii/json 'string->json) => '((string? -> any?)))
(check (hint-liii/json 'json-string-escape) => '((string? -> string?)))
(check (hint-liii/json 'json-object?) => '((any? -> boolean?)))
(check (hint-liii/json 'json-keys) => '((any? -> list?)))
(check (hint-liii/json 'json-contains-key?) => '((any? any? -> boolean?)))
(check (hint-liii/json 'json-get-or-else) => '((any? any? -> any?)))
(check (hint-liii/json 'json-ref-string)
  =>
  '((any? any? (or #f string?) -> (or #f string?)))
) ;check
(check (hint-liii/json 'json-ref-number)
  =>
  '((any? any? (or #f number?) -> (or #f number?)))
) ;check
(check (hint-liii/json 'json-ref-integer)
  =>
  '((any? any? (or #f integer?) -> (or #f integer?)))
) ;check
(check (hint-liii/json 'json-ref-boolean)
  =>
  '((any? any? (or #f boolean?) -> (or #f boolean?)))
) ;check

;; json->string 由 define* 定义：1 个必选参数 + 4 个可选参数，
;; 可接受 1~5 个参数（可选参数也可按关键字传入）：
;; indent 为 #f / 整数 / 缩进字符串，其余三个为布尔值
(check (hint-liii/json 'json->string)
  =>
  '((any? -> string?)
    (any? (or #f integer? string?) -> string?)
    (any? (or #f integer? string?) boolean? -> string?)
    (any? (or #f integer? string?) boolean? boolean? -> string?)
    (any? (or #f integer? string?) boolean? boolean? boolean? -> string?))
) ;check

;; 可变参数过程登记多种调用形态
(check (hint-liii/json 'json-ref)
  =>
  '((any? any? -> any?) (any? any? any? -> any?))
) ;check
(check (hint-liii/json 'json-set)
  =>
  '((any? any? any? -> any?) (any? any? any? any? -> any?))
) ;check
(check (hint-liii/json 'json-push)
  =>
  '((any? any? any? -> any?) (any? any? any? any? -> any?))
) ;check
(check (hint-liii/json 'json-drop)
  =>
  '((any? any? -> any?) (any? any? any? -> any?))
) ;check
(check (hint-liii/json 'json-reduce)
  =>
  '((any? any? procedure? -> any?) (any? any? procedure? any? -> any?))
) ;check

;; read-json 既可无参读取当前输入端口，也可显式指定输入端口
(check (hint-liii/json 'read-json) => '((-> any?) (input-port? -> any?)))

(check-report)
