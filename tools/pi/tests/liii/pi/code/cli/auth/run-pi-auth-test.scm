;; run-pi-auth
;; 运行 Pi auth 子命令。
;;
;; 语法
;; ----
;; (run-pi-auth args)
;; (run-pi-auth args config-dir)
;; (run-pi-auth args config-dir custom-env)
;;
;; 参数
;; ----
;; args : list?
;; auth 子命令参数字符串列表。
;;
;; config-dir : any?
;; 可选，配置目录路径（默认为 (gf-get-config-dir)）。
;;
;; custom-env : list?
;; 可选，自定义环境变量列表。
;;
;; 返回值
;; ----
;; integer?
;; 退出码：成功为 0，错误或未找到为 1。

(import (liii check)
  (liii path)
  (liii string)
  (liii tempfile)
  (liii pi code cli auth)
) ;import

(check-set-mode! 'report-failed)

(define (with-stderr-to-string thunk)
  (let* ((p (open-output-string))
         (orig *stderr*)
        ) ;
    (dynamic-wind
      (lambda () (set! *stderr* p))
      thunk
      (lambda () (set! *stderr* orig))
    ) ;dynamic-wind
    (get-output-string p)
  ) ;let*
) ;define

;; 1. 无参数或 --help 输出使用方法，返回 0
(let ((status #f))
  (let ((out (with-output-to-string (lambda () (set! status (run-pi-auth '()))))))
    (check status => 0)
    (check-true (string-contains? out "Usage: gf pi auth print-api-key --provider <provider>"))
  ) ;let
) ;let

(let ((status #f))
  (let ((out (with-output-to-string (lambda () (set! status (run-pi-auth '("--help")))))))
    (check status => 0)
    (check-true (string-contains? out "Usage: gf pi auth print-api-key --provider <provider>"))
  ) ;let
) ;let

;; 2. 正确执行 print-api-key 输出生效 key
(let* ((tmp (make-temp-dir "gf-auth-test-" ""))
       (auth-file (path-join tmp "auth.json"))
      ) ;
  (path-write-text auth-file
    "{\"deepseek\":{\"type\":\"api_key\",\"key\":\"sk-fixture-1280\"}}"
  ) ;path-write-text
  (let ((status #f))
    (let ((out (with-output-to-string
                 (lambda ()
                   (set! status (run-pi-auth '("print-api-key" "--provider" "deepseek") tmp))))))
      (check status => 0)
      (check out => "sk-fixture-1280\n")
    ) ;let
  ) ;let
  (path-unlink auth-file)
  (path-rmdir tmp)
) ;let*

;; 3. 缺失 --provider 时 stderr 提示并返回 1
(let* ((tmp (make-temp-dir "gf-auth-test-" "")))
  (let ((status #f) (err ""))
    (set! err (with-stderr-to-string
                (lambda () (set! status (run-pi-auth '("print-api-key") tmp)))))
    (check status => 1)
    (check-true (string-contains? err "--provider <provider>"))
  ) ;let
  (path-rmdir tmp)
) ;let*

;; 4. 未知子命令时 stderr 提示并返回 1
(let* ((tmp (make-temp-dir "gf-auth-test-" "")))
  (let ((status #f) (err ""))
    (set! err (with-stderr-to-string
                (lambda ()
                  (set! status
                    (run-pi-auth '("print-bearer-token" "--provider" "deepseek") tmp)
                  ) ;set!
                ) ;lambda
              ) ;with-stderr-to-string
    ) ;set!
    (check status => 1)
    (check-true (string-contains? err "print-api-key"))
  ) ;let
  (path-rmdir tmp)
) ;let*

;; 5. 未知选项时 stderr 提示并返回 1
(let* ((tmp (make-temp-dir "gf-auth-test-" "")))
  (let ((status #f) (err ""))
    (set! err (with-stderr-to-string
                (lambda ()
                  (set! status
                    (run-pi-auth '("print-api-key" "--provider" "deepseek" "--model" "m") tmp)
                  ) ;set!
                ) ;lambda
              ) ;with-stderr-to-string
    ) ;set!
    (check status => 1)
    (check-true (string-contains? err "unknown option"))
  ) ;let
  (path-rmdir tmp)
) ;let*

(check-report)
