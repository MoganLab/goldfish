;; pi-app-model-catalog
;; 从配置目录重建模型列表与默认模型快照（纯函数）。
;;
;; 语法
;; ----
;; (pi-app-model-catalog [config-dir]) -> (values models default-model)
;;
;; 参数
;; ----
;; config-dir : string? 或 path?，可选
;; 配置目录，省略时默认为 (gf-get-config-dir)。
;;
;; 返回值
;; ----
;; (values models default-model)
;; models : list
;; 已激活模型列表，按 provider/id 升序排序。
;; default-model : list 或 #f
;; 默认模型键值对 ((provider . "p") (id . "m"))，settings.json 缺失或未配置时为 #f。

(import (scheme base)
  (srfi srfi-1)
  (liii check)
  (liii os)
  (liii path)
  (liii list)
  (liii sort)
  (liii sys)
  (liii pi code modes interactive app-model)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE-DIR
  (path->string
    (path-absolute (path (if (file-exists? "tests/fixtures/user-config/config-tui")
                           "tests/fixtures/user-config/config-tui"
                           "tools/pi/tests/fixtures/user-config/config-tui"
                         ) ;if
                   ) ;path
    ) ;path-absolute
  ) ;path->string
) ;define

(define (with-config-dir dir thunk)
  (let ((saved (getenv "GF_CONFIG_DIR")) (saved-pi (getenv "PI_CODING_AGENT_DIR")))
    (putenv "GF_CONFIG_DIR" dir)
    (unsetenv "PI_CODING_AGENT_DIR")
    (let ((result (thunk)))
      (if saved (putenv "GF_CONFIG_DIR" saved) (unsetenv "GF_CONFIG_DIR"))
      (when saved-pi
        (putenv "PI_CODING_AGENT_DIR" saved-pi)
      ) ;when
      result
    ) ;let
  ) ;let
) ;define

;; 1. GF_CONFIG_DIR 指向 config-tui 夹具，无参调用重建目录：
;;    包含白名单内 provider（google、kimi-coding、custom-kimi），不含白名单外 provider，按 provider 与 id 排序；
;;    default-model 取自 settings.json 的 defaultProvider 与 defaultModel
(with-config-dir FIXTURE-DIR
  (lambda ()
    (let-values (((models dm) (pi-app-model-catalog)))
      ;; 默认模型解析：kimi-coding / k3.1-pro
      (check dm => '((provider . "kimi-coding") (id . "k3.1-pro")))

      ;; 模型列表非空且包含白名单内的所有 provider
      (check-true (pair? models))
      (let ((providers
              (delete-duplicates
                (map (lambda (m) (cdr (assoc 'provider m))) models)
              ) ;delete-duplicates
            ) ;providers
            (keys
              (map
                (lambda (m) (string-append (cdr (assoc 'provider m)) "/" (cdr (assoc 'id m))))
                models
              ) ;map
            ) ;keys
           ) ;
        (check providers => '("custom-kimi" "google" "kimi-coding"))
        ;; 不含白名单外 provider（如 deepseek、minimax-cn、zai-coding-cn）
        (check-false
          (any
            (lambda (m) (string=? (cdr (assoc 'provider m)) "deepseek"))
            models
          ) ;any
        ) ;check-false
        (check-false
          (any
            (lambda (m) (string=? (cdr (assoc 'provider m)) "minimax-cn"))
            models
          ) ;any
        ) ;check-false
        (check-false
          (any
            (lambda (m) (string=? (cdr (assoc 'provider m)) "zai-coding-cn"))
            models
          ) ;any
        ) ;check-false
        ;; 按 provider/id 升序排序
        (check keys => (list-sort string<? keys))
      ) ;let
    ) ;let-values
  ) ;lambda
) ;with-config-dir

;; 2. 显式传参：(pi-app-model-catalog FIXTURE-DIR) 结果与无参注入一致
(let-values (((models dm) (pi-app-model-catalog FIXTURE-DIR)))
  (check dm => '((provider . "kimi-coding") (id . "k3.1-pro")))
  (check-true (pair? models))
  (let ((providers
          (delete-duplicates
            (map (lambda (m) (cdr (assoc 'provider m))) models)
          ) ;delete-duplicates
        ) ;providers
       ) ;
    (check providers => '("custom-kimi" "google" "kimi-coding"))
  ) ;let
) ;let-values

;; 3. settings.json 缺失或配置不全时，default-model 解析为 #f
(let-values (((models dm) (pi-app-model-catalog "/nonexistent-dir-for-test")))
  (check dm => #f)
) ;let-values

(check-report)
