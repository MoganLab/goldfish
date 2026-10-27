(import (liii check) (liii pi code core extensions))

(check-set-mode! 'report-failed)

;; make-extension-flag-def
;; 创建扩展命令行参数定义，并推导参数键及是否接收参数值。
;;
;; 语法
;; ----
;; (make-extension-flag-def flag description . options) -> extension-flag-def
;;
;; 参数
;; ----
;; flag : string
;; 非空参数名，例如 "--verbose"。
;; description : string
;; 参数说明。
;; options : list
;; 可选 :handler、:type、:default、:key、:aliases、:takes-arg?、:validator。
;;
;; 返回值
;; ----
;; extension-flag-def
;; 未提供 key 时去除 flag 开头的连字符并转换为 symbol；全为连字符时使用 'flag。
;; 非 boolean 类型自动视为需要参数值。
;;
;; 示例
;; ----
;; (extension-flag-def-key (make-extension-flag-def "--verbose" "Verbose"))
;; => 'verbose

(let ((flag (make-extension-flag-def "--verbose" "Verbose output")))
  (check (extension-flag-def? flag) => #t)
  (check (extension-flag-def-flag flag) => "--verbose")
  (check (extension-flag-def-key flag) => 'verbose)
  (check (extension-flag-def-takes-arg? flag) => #f)
) ;let

(check (extension-flag-def-key (make-extension-flag-def "---" "Marker"))
  =>
  'flag
) ;check
(check (extension-flag-def-key (make-extension-flag-def "--output" "Output" :key 'destination)
       ) ;extension-flag-def-key
  =>
  'destination
) ;check
(check (extension-flag-def-takes-arg? (make-extension-flag-def "--model" "Model" :type 'string)
       ) ;extension-flag-def-takes-arg?
  =>
  #t
) ;check
(check-catch 'type-error (make-extension-flag-def "" "Invalid"))
(check-catch 'type-error (make-extension-flag-def "--x" 123))

(check-report)
