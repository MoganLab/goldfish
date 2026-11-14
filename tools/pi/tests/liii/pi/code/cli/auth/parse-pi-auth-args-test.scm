;; parse-pi-auth-args
;; 解析 auth 子命令的参数列表。
;;
;; 语法
;; ----
;; (parse-pi-auth-args args)
;;
;; 参数
;; ----
;; args : list?
;; 命令行传入 auth 子命令之后的参数字符串列表。
;;
;; 返回值
;; ----
;; <pi-auth-args>
;; 解析后的参数记录（包含 help?、subcommand、provider、key 字段）。

(import (liii check) (liii pi code cli auth))

(check-set-mode! 'report-failed)

;; 1. 无参数或 --help/-h 时返回 help? 为 #t 的记录
(for-each
  (lambda (argv)
    (let ((res (parse-pi-auth-args argv)))
      (check-true (pi-auth-args? res))
      (check-true (pi-auth-args-help? res))
    ) ;let
  ) ;lambda
  '(() ("-h") ("--help") ("print-api-key" "--help") ("print-api-key" "-h"))
) ;for-each

;; 2. 正确识别 print-api-key 与 --provider 参数
(let ((res (parse-pi-auth-args '("print-api-key" "--provider" "deepseek"))))
  (check-true (pi-auth-args? res))
  (check (pi-auth-args-help? res) => #f)
  (check (pi-auth-args-subcommand res) => "print-api-key")
  (check (pi-auth-args-provider res) => "deepseek")
  (check (pi-auth-args-key res) => #f)
) ;let

;; 3. 支持 --provider=value 格式
(let ((res (parse-pi-auth-args '("print-api-key" "--provider=deepseek"))))
  (check-true (pi-auth-args? res))
  (check (pi-auth-args-subcommand res) => "print-api-key")
  (check (pi-auth-args-provider res) => "deepseek")
) ;let

;; 4. 缺失 --provider 时报错
(check-catch 'pi-arg-error (parse-pi-auth-args '("print-api-key")))
(check-catch 'pi-arg-error (parse-pi-auth-args '("print-api-key" "--provider")))
(check-catch 'pi-arg-error (parse-pi-auth-args '("print-api-key" "--provider=")))
(check-catch 'pi-arg-error (parse-pi-auth-args '("print-api-key" "--provider" "")))

;; 5. 未知子命令报错
(check-catch 'pi-arg-error
  (parse-pi-auth-args '("print-bearer-token" "--provider" "deepseek"))
) ;check-catch

;; 6. 未知选项报错
(check-catch 'pi-arg-error
  (parse-pi-auth-args '("print-api-key" "--provider" "deepseek" "--model" "m"))
) ;check-catch
(check-catch 'pi-arg-error
  (parse-pi-auth-args '("print-api-key" "--provider" "deepseek" "--unknown"))
) ;check-catch

;; 7. 多余位置参数报错
(check-catch 'pi-arg-error
  (parse-pi-auth-args '("print-api-key" "--provider" "deepseek" "extra"))
) ;check-catch

(check-report)
