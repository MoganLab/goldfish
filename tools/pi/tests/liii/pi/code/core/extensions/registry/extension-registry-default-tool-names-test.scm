(import (liii check) (liii pi code core extensions))

(check-set-mode! 'report-failed)

;; extension-registry-default-tool-names
;; 按注册顺序返回所有声明为默认激活的扩展工具名称。
;;
;; 语法
;; ----
;; (extension-registry-default-tool-names target) -> list
;;
;; 参数
;; ----
;; target : extension-registry | extension-api | extension-context
;; 扩展注册表或能够定位注册表的对象。
;;
;; 返回值
;; ----
;; list
;; 字符串工具名列表；仅包含 default-active? 为真的工具，空注册表返回空列表。
;;
;; 示例
;; ----
;; (extension-registry-default-tool-names registry) => '("web_search")

(let ((registry (make-extension-registry)))
  (check (extension-registry-default-tool-names registry) => '())
) ;let

(let ((registry (make-extension-registry)))
  (register-tool! registry
    (make-extension-tool-def "first"
      "default"
      '()
      (lambda (args) #t)
      :default-active? #t
    ) ;make-extension-tool-def
  ) ;register-tool!
  (register-tool! registry
    (make-extension-tool-def "optional" "optional" '() (lambda (args) #t))
  ) ;register-tool!
  (register-tool! registry
    (make-extension-tool-def "last"
      "default"
      '()
      (lambda (args) #t)
      :default-active? #t
    ) ;make-extension-tool-def
  ) ;register-tool!
  (check (extension-registry-default-tool-names registry) => '("first" "last"))
) ;let

(check-report)
