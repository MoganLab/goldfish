(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii pi ai keyring)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi code core extensions)
  (liii pi multikey config)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; 回归：会话内 /model 跨 provider 切换后必须重新执行 config augmenter，
;; 否则从无 multikey pool 的默认 provider 切到有 pool 的 provider 时，
;; 请求路径拿不到 request-key / rotate-key 钩子，多 key 轮换失效。

(define TMP-AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-cfg-with-model-multikey-test"))
) ;define

(when (file-exists? TMP-AGENT-DIR)
  (rmtree TMP-AGENT-DIR)
) ;when

(path-mkdir TMP-AGENT-DIR :parents #t :exist-ok #t)

(define (tmp-file name)
  (path->string (path-join (path TMP-AGENT-DIR) (path name)))
) ;define

(path-write-text (tmp-file "auth.json")
  "{\"google\":{\"type\":\"api_key\",\"key\":\"fixture-google-key\"},\"kimi-coding\":{\"type\":\"api_key\",\"key\":\"fixture-kimi-auth-key\"}}"
) ;path-write-text

(path-write-text (tmp-file "providers.json") "[\"google\", \"kimi-coding\"]")

;; 只有 kimi-coding 有池，google 没有：用于验证跨 provider 双向切换
(path-write-text (tmp-file "multikey.json")
  "{\"pools\":[{\"id\":\"kimi-coding\",\"cooldownMs\":20000,\"keys\":[{\"key\":\"test-kimi-key-1\",\"label\":\"k1\"},{\"key\":\"test-kimi-key-2\",\"label\":\"k2\"}]}]}"
) ;path-write-text

(define reg (make-extension-registry))
(register-config-augmenter! reg pi-multikey-augment-config)

(define (with-reg thunk)
  (with-extension-registry reg thunk)
) ;define

(let* ((env (list (cons "PI_CODING_AGENT_DIR" TMP-AGENT-DIR)))
       (parsed (parse-pi-args '()))
       (base (resolve-pi-config parsed env))
       (startup (with-reg (lambda () (augment-pi-config base parsed))))
      ) ;
  ;; 默认 provider（google）无 pool：启动时不应有轮换钩子
  (check (cdr (assoc 'provider startup)) => "google")
  (check-false (pi-key-rotation? startup))

  ;; /model 切到 kimi-coding：重新增强后注入池与钩子
  (let ((kimi
          (with-reg (lambda ()
                      (%worker-job-cfg startup (list "hi" "medium" "kimi-coding" "k3.1-pro") parsed)
                    ) ;lambda
          ) ;with-reg
        ) ;kimi
       ) ;
    (check (cdr (assoc 'provider kimi)) => "kimi-coding")
    (check-true (pi-key-rotation? kimi))
    (check (pi-request-api-key kimi) => "test-kimi-key-1")

    ;; 同 provider 的下一条消息不重建池：429 冷却状态必须保留
    (check (pi-rotate-api-key! kimi "test-kimi-key-1" 429) => "test-kimi-key-2")
    (let ((kimi-again
            (with-reg (lambda ()
                        (%worker-job-cfg kimi (list "hi" "medium" "kimi-coding" "k3.1-pro") parsed)
                      ) ;lambda
            ) ;with-reg
          ) ;kimi-again
         ) ;
      (check-true (pi-key-rotation? kimi-again))
      (check (pi-request-api-key kimi-again) => "test-kimi-key-2")

      ;; 再切回无 pool 的 google：旧池与钩子必须被清理，回退 auth.json key
      (let ((google
              (with-reg (lambda ()
                          (%worker-job-cfg kimi-again
                            (list "hi" "medium" "google" "gemini-3.8-flash")
                            parsed
                          ) ;%worker-job-cfg
                        ) ;lambda
              ) ;with-reg
            ) ;google
           ) ;
        (check (cdr (assoc 'provider google)) => "google")
        (check-false (assoc 'multikey-pool google))
        (check-false (pi-key-rotation? google))
        (check (pi-request-api-key google) => "fixture-google-key")
      ) ;let
    ) ;let
  ) ;let
) ;let*

(rmtree TMP-AGENT-DIR)

(check-report)
