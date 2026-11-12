;; pi-auth-print-api-key
;; 解析并打印指定 provider 的生效 API 密钥。
;;
;; 语法
;; ----
;; (pi-auth-print-api-key provider)
;; (pi-auth-print-api-key provider config-dir)
;; (pi-auth-print-api-key provider config-dir opt-env)
;;
;; 参数
;; ----
;; provider : string?
;; 服务商标识名称（如 "deepseek"、"google" 等）。
;;
;; config-dir : any?
;; 可选，配置目录路径（默认为 (gf-get-config-dir)）。
;;
;; opt-env : list?
;; 可选，自定义环境变量列表。
;;
;; 返回值
;; ----
;; integer?
;; 退出码：成功输出密钥返回 0，未找到或失败返回 1。

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

;; 1. auth.json 中存在 key 时正常打印到 stdout，退出码为 0
(let* ((tmp (make-temp-dir "gf-auth-test-" ""))
       (auth-file (path-join tmp "auth.json"))
      ) ;
  (path-write-text auth-file
    "{\"deepseek\":{\"type\":\"api_key\",\"key\":\"sk-fixture-1280\"}}"
  ) ;path-write-text
  (let ((status #f))
    (let ((out (with-output-to-string
                 (lambda () (set! status (pi-auth-print-api-key "deepseek" tmp '()))))))
      (check status => 0)
      (check out => "sk-fixture-1280\n")
    ) ;let
  ) ;let
  (path-unlink auth-file)
  (path-rmdir tmp)
) ;let*

;; 2. 环境变量优先于 auth.json
(let* ((tmp (make-temp-dir "gf-auth-test-" ""))
       (auth-file (path-join tmp "auth.json"))
       (env '(("DEEPSEEK_API_KEY" . "sk-env-1280")))
      ) ;
  (path-write-text auth-file
    "{\"deepseek\":{\"type\":\"api_key\",\"key\":\"sk-fixture-1280\"}}"
  ) ;path-write-text
  (let ((status #f))
    (let ((out (with-output-to-string
                 (lambda () (set! status (pi-auth-print-api-key "deepseek" tmp env))))))
      (check status => 0)
      (check out => "sk-env-1280\n")
    ) ;let
  ) ;let
  (path-unlink auth-file)
  (path-rmdir tmp)
) ;let*

;; 3. ${VAR} 环境变量模板插值
(let* ((tmp (make-temp-dir "gf-auth-test-" ""))
       (auth-file (path-join tmp "auth.json"))
       (env '(("MY_CUSTOM_VAR" . "sk-interpolated-1280")))
      ) ;
  (path-write-text auth-file
    "{\"deepseek\":{\"type\":\"api_key\",\"key\":\"${MY_CUSTOM_VAR}\"}}"
  ) ;path-write-text
  (let ((status #f))
    (let ((out (with-output-to-string
                 (lambda () (set! status (pi-auth-print-api-key "deepseek" tmp env))))))
      (check status => 0)
      (check out => "sk-interpolated-1280\n")
    ) ;let
  ) ;let
  (path-unlink auth-file)
  (path-rmdir tmp)
) ;let*

;; 4. 未找到 key 时输出说明到 stderr，stdout 无输出，退出码为 1
(let* ((tmp (make-temp-dir "gf-auth-test-empty-" "")))
  (let ((status #f)
        (out "")
        (err "")
       ) ;
    (set! err (with-stderr-to-string
                (lambda ()
                  (set! out (with-output-to-string
                              (lambda () (set! status (pi-auth-print-api-key "kimi-coding" tmp '()))))))))
    (check status => 1)
    (check out => "")
    (check-true (string-contains? err "kimi-coding"))
    (check-true (string-contains? err "auth.json"))
    (check-true (string-contains? err "KIMI_API_KEY"))
  ) ;let
  (path-rmdir tmp)
) ;let*

;; 5. 不受支持的 provider（如 all-in-one 或未知 id）同样走未找到说明路径
(let* ((tmp (make-temp-dir "gf-auth-test-unknown-" "")))
  (let ((status #f)
        (out "")
        (err "")
       ) ;
    (set! err (with-stderr-to-string
                (lambda ()
                  (set! out (with-output-to-string
                              (lambda () (set! status (pi-auth-print-api-key "all-in-one" tmp '()))))))))
    (check status => 1)
    (check out => "")
    (check-true (string-contains? err "all-in-one"))
    (check-true (string-contains? err "auth.json"))
  ) ;let
  (path-rmdir tmp)
) ;let*

;; 6. ollama 免密回退到 "ollama"
(let* ((tmp (make-temp-dir "gf-auth-test-ollama-" "")))
  (let ((status #f))
    (let ((out (with-output-to-string
                 (lambda () (set! status (pi-auth-print-api-key "ollama" tmp '()))))))
      (check status => 0)
      (check out => "ollama\n")
    ) ;let
  ) ;let
  (path-rmdir tmp)
) ;let*

;; 7. 服务商别名支持（zai-coding-cn 与 zai）
(let* ((tmp (make-temp-dir "gf-auth-test-alias-" ""))
       (auth-file (path-join tmp "auth.json"))
      ) ;
  (path-write-text auth-file
    "{\"zai\":{\"type\":\"api_key\",\"key\":\"sk-zai-alias\"}}"
  ) ;path-write-text
  (let ((status #f))
    (let ((out (with-output-to-string
                 (lambda () (set! status (pi-auth-print-api-key "zai-coding-cn" tmp '()))))))
      (check status => 0)
      (check out => "sk-zai-alias\n")
    ) ;let
  ) ;let
  (path-unlink auth-file)
  (path-rmdir tmp)
) ;let*

(check-report)
