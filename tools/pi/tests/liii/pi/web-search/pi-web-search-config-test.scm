(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii os)
  (liii pi web-search config)
) ;import

(check-set-mode! 'report-failed)

;; 辅助环境隔离过程

(define (with-test-env vars-alist thunk)
  (let ((saved-vars
          (map
            (lambda (p) (cons (car p) (get-environment-variable (car p))))
            vars-alist
          ) ;map
        ) ;saved-vars
       ) ;
    (dynamic-wind
      (lambda ()
        (for-each
          (lambda (p) (if (cdr p) (putenv (car p) (cdr p)) (unsetenv (car p))))
          vars-alist
        ) ;for-each
      ) ;lambda
      thunk
      (lambda ()
        (for-each
          (lambda (p) (if (cdr p) (putenv (car p) (cdr p)) (unsetenv (car p))))
          saved-vars
        ) ;for-each
      ) ;lambda
    ) ;dynamic-wind
  ) ;let
) ;define

;; 1. pi-valid-key? 有效性谓词测试
(check-true (pi-valid-key? "abc"))
(check-true (pi-valid-key? "  abc  "))
(check-false (pi-valid-key? ""))
(check-false (pi-valid-key? "   "))
(check-false (pi-valid-key? #f))
(check-false (pi-valid-key? 123))
(check-false (pi-valid-key? '()))

;; 2. pi-resolve-google-api-key 凭据解析与回退测试
;; 测试项1：仅设置 GOOGLE_API_KEY 环境变量时正确解析凭据（即使 config 中存在 ('api-key . #f)）
(with-test-env '(("GOOGLE_API_KEY" . "test-env-key-123")
                 ("GEMINI_API_KEY" . #f))
  (lambda ()
    (check (pi-resolve-google-api-key (list (cons 'api-key #f)) #f)
      =>
      "test-env-key-123"
    ) ;check
    (check (pi-resolve-google-api-key (list (cons "api-key" #f)) #f)
      =>
      "test-env-key-123"
    ) ;check
    (check (pi-resolve-google-api-key (list (cons 'api-key "")) #f)
      =>
      "test-env-key-123"
    ) ;check
    (check (pi-resolve-google-api-key (list (cons 'api-key "   ")) #f)
      =>
      "test-env-key-123"
    ) ;check
    (check (pi-resolve-google-api-key (list (cons 'provider "google") (cons 'api-key #f))
             #f
           ) ;pi-resolve-google-api-key
      =>
      "test-env-key-123"
    ) ;check
    (check (pi-resolve-google-api-key '() #f) => "test-env-key-123")
  ) ;lambda
) ;with-test-env

;; 测试项2：仅设置 GEMINI_API_KEY 环境变量时正确解析凭据
(with-test-env '(("GOOGLE_API_KEY" . #f)
                 ("GEMINI_API_KEY" . "test-gemini-key-456"))
  (lambda ()
    (check (pi-resolve-google-api-key (list (cons 'api-key #f)) #f)
      =>
      "test-gemini-key-456"
    ) ;check
    (check (pi-resolve-google-api-key '() #f) => "test-gemini-key-456")
  ) ;lambda
) ;with-test-env

;; 测试项2.1：GOOGLE_API_KEY 为空字符串时能回退到 GEMINI_API_KEY
(with-test-env '(("GOOGLE_API_KEY" . "")
                 ("GEMINI_API_KEY" . "test-gemini-key-456"))
  (lambda ()
    (check (pi-resolve-google-api-key (list (cons 'api-key #f)) #f)
      =>
      "test-gemini-key-456"
    ) ;check
  ) ;lambda
) ;with-test-env

;; 测试项3：显式有效配置凭据具有优先权
(with-test-env '(("GOOGLE_API_KEY" . "env-key")
                 ("GEMINI_API_KEY" . "gemini-env-key"))
  (lambda ()
    (check (pi-resolve-google-api-key (list (cons 'api-key "config-key")) #f)
      =>
      "config-key"
    ) ;check
    (check (pi-resolve-google-api-key (list (cons "api-key" "config-key")) #f)
      =>
      "config-key"
    ) ;check
    (check (pi-resolve-google-api-key (list (cons 'provider "google") (cons 'api-key "config-key"))
             #f
           ) ;pi-resolve-google-api-key
      =>
      "config-key"
    ) ;check
  ) ;lambda
) ;with-test-env

;; 测试项4：非 google provider 的 api-key 不会被 google web_search 误用，透明回退到环境变量
(with-test-env '(("GOOGLE_API_KEY" . "google-env-key") ("GEMINI_API_KEY" . #f))
  (lambda ()
    (check (pi-resolve-google-api-key (list (cons 'provider "anthropic") (cons 'api-key "sk-ant-test"))
             #f
           ) ;pi-resolve-google-api-key
      =>
      "google-env-key"
    ) ;check
  ) ;lambda
) ;with-test-env

;; 测试项5：opt-key 参数优先级最高
(with-test-env '(("GOOGLE_API_KEY" . "env-key"))
  (lambda ()
    (check (pi-resolve-google-api-key (list (cons 'api-key "config-key")) "opt-key")
      =>
      "opt-key"
    ) ;check
    (check (pi-resolve-google-api-key (list (cons 'api-key "config-key")) "") => #f)
  ) ;lambda
) ;with-test-env

(check-report)
