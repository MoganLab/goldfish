;; multikey-extension
;; 验证内置 multikey 扩展脚本的加载与配置增强器（config augmenter）注入。
;;
;; 语法
;; ----
;; (pi-load-extension path api)
;;
;; 参数
;; ----
;; path : string?
;; 扩展脚本路径。
;;
;; api : extension-api?
;; 扩展宿主 API 实例。
;;
;; 返回值
;; ----
;; list?
;; 扩展加载结果字典。

(import (liii check)
  (liii pi code core extensions)
  (liii pi code config)
  (liii pi ai keyring)
  (liii pi multikey config)
) ;import

(check-set-mode! 'report-failed)

;; multikey 内置扩展（extensions/multikey.scm）：
;; 注册 config augmenter，命中 multikey.json 的 pool 时增强 cfg 注入轮换钩子。

(define (get-multikey-path)
  (cond
    ((file-exists? "tools/pi/extensions/multikey.scm")
     "tools/pi/extensions/multikey.scm")
    ((file-exists? "extensions/multikey.scm")
     "extensions/multikey.scm")
    (else "../../extensions/multikey.scm")
  ) ;cond
) ;define

(define fixture-dir
  (cond
    ((file-exists? "tests/fixtures/multikey") "tests/fixtures/multikey")
    ((file-exists? "tools/pi/tests/fixtures/multikey") "tools/pi/tests/fixtures/multikey")
    (else "../../tests/fixtures/multikey")
  ) ;cond
) ;define

;; 1. 加载 multikey.scm 扩展并验证 augmenter 注册
(let* ((api (make-extension-api))
       (res (pi-load-extension (get-multikey-path) api))
      ) ;
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'ok) => #t)
  (check (cdr (assq 'value res)) => "multikey-loaded")
  (check (length (extension-registry-augmenters-list api)) => 1)

  ;; 2. augment-pi-config 在该注册表下增强 cfg：注入轮换钩子并换首个池内 key
  (let* ((base (list (cons 'agent-dir fixture-dir)
                     (cons 'provider "google")
                     (cons 'api-key "auth-json-key")
                ) ;list
           ) ;base
         (cfg (with-extension-registry api
                (lambda () (augment-pi-config base '()))
              ) ;with-extension-registry
         ) ;cfg
        ) ;
    (check (cdr (assoc 'api-key cfg)) => "goog-key-0000000001")
    (check-true (pi-key-rotation? cfg))
    (check (pi-request-api-key cfg) => "goog-key-0000000001")
    (check (pi-rotate-api-key! cfg "goog-key-0000000001" 429)
      => "goog-key-0000000002"
    ) ;check
  ) ;let*

  ;; 3. 重复加载幂等（augmenter 追加式注册，不重复增强）
  (pi-load-extension (get-multikey-path) api)
  (check (length (extension-registry-augmenters-list api)) => 1)
) ;let*

(check-report)
