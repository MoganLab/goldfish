;; embedded-resource
;; 在根资源中查找内嵌的资源。
;;
;; 语法
;; ----
;; (embedded-resource root uri)
;;
;; 参数
;; ----
;; root : any?
;; 根资源。
;;
;; uri : string
;; 内嵌资源的绝对 $id。
;;
;; 返回值
;; ----
;; any? | boolean
;; 找到时返回该资源的 Schema 对象，未找到时返回 #f。
;;
;; 注意
;; ----
;; 只沿着子 Schema 位置的关键字向下查找，
;; 名称到 Schema 的映射关键字按每个值单独查找。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema embedded))

(check-set-mode! 'report-failed)

(define document
  '(("$id" . "https://example.com/root/doc.json")
    ("$defs"
     ("x" ("$id" . "https://example.com/root/def.json") ("type" . "integer")))
    ("properties" ("p" ("$id" . "https://example.com/root/prop.json"))))
) ;define

;; $defs 下的内嵌资源。

(check (embedded-resource document "https://example.com/root/def.json")
  =>
  '(("$id" . "https://example.com/root/def.json") ("type" . "integer"))
) ;check

;; properties 下的内嵌资源。

(check (embedded-resource document "https://example.com/root/prop.json")
  =>
  '(("$id" . "https://example.com/root/prop.json"))
) ;check

;; 找不到时返回 #f。

(check-false (embedded-resource document "https://example.com/root/missing.json")
) ;check-false

(check-report)
