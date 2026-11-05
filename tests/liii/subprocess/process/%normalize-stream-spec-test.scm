;; %normalize-stream-spec
;; 校验并归一化重定向流规格参数。
;;
;; 语法
;; ----
;; (%normalize-stream-spec caller kw val allowed-symbols default)
;;
;; 参数
;; ----
;; caller : symbol 或 string
;; 调用方名称，用于错误提示信息。
;;
;; kw : keyword 或 symbol
;; 参数关键字名称（如 :stdin、:stdout、:stderr）。
;;
;; val : any
;; 待校验的重定向规格值（#f、path、string 或 symbol）。
;;
;; allowed-symbols : list
;; 允许的符号列表（例如 '(:pipe :inherit :null)）。
;;
;; default : any
;; 当 val 为 #f 时返回的默认值。
;;
;; 返回值
;; ------
;; string、symbol 或 default
;; 归一化后的流规格配置。
;;
;; 说明
;; ----
;; - 若 val 为 #f，返回 default。
;; - 若 val 为 path 对象，转换为其字符串路径表示。
;; - 若 val 为 string，直接返回原字符串。
;; - 若 val 为 symbol，必须在 allowed-symbols 列表中，否则抛出 value-error。
;; - 若 val 为其他不受支持的类型，抛出 type-error。

(import (liii check) (liii path) (liii subprocess process))

(check-set-mode! 'report-failed)

;; val 为 #f 时返回 default
(check (%normalize-stream-spec 'spawn :stdin #f '(:pipe :inherit) :pipe)
  =>
  :pipe
) ;check

;; string 直接原样返回
(check (%normalize-stream-spec 'spawn
         :stdout "/tmp/out.log"
         '(:pipe :inherit)
         :inherit
       ) ;%normalize-stream-spec
  =>
  "/tmp/out.log"
) ;check

;; path 对象转换为 string
(check (%normalize-stream-spec 'spawn
         :stderr (path "/tmp/err.log")
         '(:pipe :null)
         :null
       ) ;%normalize-stream-spec
  =>
  "/tmp/err.log"
) ;check

;; symbol 在 allowed-symbols 列表中时直接返回
(check (%normalize-stream-spec 'spawn :stdin :pipe '(:pipe :inherit :null) :inherit)
  =>
  :pipe
) ;check
(check (%normalize-stream-spec 'spawn :stdout :inherit '(:pipe :inherit :null) :pipe)
  =>
  :inherit
) ;check

;; symbol 不在 allowed-symbols 中时抛出 value-error
(check-catch 'value-error
  (%normalize-stream-spec 'spawn
    :stdin :invalid-sym
    '(:pipe :inherit :null)
    :pipe
  ) ;%normalize-stream-spec
) ;check-catch

;; 其他非法类型抛出 type-error
(check-catch 'type-error
  (%normalize-stream-spec 'spawn :stdout 12345 '(:pipe :inherit) :pipe)
) ;check-catch
(check-catch 'type-error
  (%normalize-stream-spec 'spawn :stdout #t '(:pipe :inherit) :pipe)
) ;check-catch
(check-catch 'type-error
  (%normalize-stream-spec 'spawn :stdout '("out.log") '(:pipe :inherit) :pipe)
) ;check-catch

(check-report)
