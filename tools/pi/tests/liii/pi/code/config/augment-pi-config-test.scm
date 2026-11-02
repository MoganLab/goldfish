;; augment-pi-config
;; 依次应用扩展注册的 config augmenter 增强运行配置。
;;
;; 语法
;; ----
;; (augment-pi-config cfg parsed)
;;
;; 参数
;; ----
;; cfg : alist
;; resolve-pi-config 生成的运行配置。
;; parsed : alist
;; 命令行参数解析结果，原样传给各 augmenter。
;;
;; 返回值
;; ----
;; alist
;; 依次经过全部 augmenter 处理后的运行配置。

(import (liii check) (liii pi code config) (liii pi code core extensions))

(check-set-mode! 'report-failed)

(define REG (make-extension-registry))

;; augmenter 可以读取 parsed 并向 cfg 注入新字段
(register-config-augmenter! REG
  (lambda (cfg parsed) (cons (cons 'injected (cdr (assoc 'model-id parsed))) cfg))
) ;register-config-augmenter!

(let* ((parsed '((model-id . "m1")))
       (cfg
         (with-extension-registry REG
           (lambda () (augment-pi-config '((model-id . "m1")) parsed))
         ) ;with-extension-registry
       ) ;cfg
      ) ;
  (check (cdr (assoc 'model-id cfg)) => "m1")
  (check (cdr (assoc 'injected cfg)) => "m1")
) ;let*

;; 第二个 augmenter 在前一个结果上继续增强
(register-config-augmenter! REG
  (lambda (cfg parsed) (cons (cons 'injected-2 (cdr (assoc 'injected cfg))) cfg))
) ;register-config-augmenter!

(let* ((parsed '((model-id . "m2")))
       (cfg
         (with-extension-registry REG
           (lambda () (augment-pi-config '((model-id . "m2")) parsed))
         ) ;with-extension-registry
       ) ;cfg
      ) ;
  (check (cdr (assoc 'injected cfg)) => "m2")
  (check (cdr (assoc 'injected-2 cfg)) => "m2")
) ;let*

;; 空注册表下原样返回 cfg
(let ((empty-reg (make-extension-registry)))
  (check
    (with-extension-registry empty-reg
      (lambda () (augment-pi-config '((model-id . "m3")) '()))
    ) ;with-extension-registry
    =>
    '((model-id . "m3"))
  ) ;check
) ;let

(check-report)
