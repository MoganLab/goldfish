;; augment-pi-config
;; 应用扩展注册的 config augmenter，按注册顺序增强解析后的配置。
;;
;; 语法
;; ----
;; (augment-pi-config cfg parsed) -> alist
;;
;; 参数
;; ----
;; cfg : alist
;; resolve-pi-config 返回的配置。
;; parsed : alist
;; parse-pi-args 解析后的命令行参数。
;;
;; 返回值
;; ----
;; alist
;; 经所有已注册 augmenter 处理后的配置。

(import (liii check) (liii pi code core extensions) (liii pi code config))

(check-set-mode! 'report-failed)

;; 无 augmenter 时原样返回
(let ((cfg (make-pi-config :provider "google")))
  (check
    (with-extension-registry (make-extension-api)
      (lambda () (augment-pi-config cfg '()))
    ) ;with-extension-registry
    =>
    cfg
  ) ;check
) ;let

;; 注册的 augmenter 会被依次应用
(let ((api (make-extension-api)))
  (set-extension-registry-augmenters! (extension-api-registry api)
    (list (lambda (cfg parsed) (pi-cfg-set cfg 'augmented #t)))
  ) ;set-extension-registry-augmenters!
  (let ((out
          (with-extension-registry api
            (lambda () (augment-pi-config (make-pi-config :provider "google") '()))
          ) ;with-extension-registry
        ) ;out
       ) ;
    (check (pi-cfg-ref out 'augmented) => #t)
    (check (pi-cfg-ref out 'provider) => "google")
  ) ;let
) ;let

(check-report)
