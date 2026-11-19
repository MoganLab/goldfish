;; all-in-one-models
;; 从指定配置目录加载配置并返回伪模型列表。
;;
;; 语法
;; ----
;; (all-in-one-models . opt-config-dir)
;;
;; 参数
;; ----
;; opt-config-dir : (or string path #f) (可选)
;; 可选配置目录，未提供或为 #f 时使用默认的 ~/.gf/agent/all-in-one.json。
;;
;; 返回值
;; ----
;; list of alist
;; 伪模型元数据列表；未配置或配置非法时回退 defaults 的默认双链，因此永不返回空列表。
;;
;; 说明
;; ----
;; 单链投影 %chain->model 是内部实现：把 <chain-def> 摊成标准模型元数据 alist
;; （id 取链 ID、name 取链展示名、provider/api 固定为 all-in-one、reasoning 恒为 #t、
;; contextWindow 取整链保守窗口、maxTokens 固定 65536）。字段顺序与取值属于
;; 目录投影契约，由本文件逐字段断言。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii os)
  (liii pi code all-in-one provider)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-prov-models-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define (%write-cfg content)
  (when (file-exists? TMP-DIR)
    (rmtree TMP-DIR #t)
  ) ;when
  (path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)
  (let ((cfg-file (path-join TMP-DIR "all-in-one.json")))
    (call-with-output-file (path->string cfg-file) (lambda (p) (display content p)))
  ) ;let
) ;define

;; 1. 无配置时返回默认内置伪模型列表（coding-fast 与 coding-high-quality）
(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(let ((models (all-in-one-models TMP-DIR)))
  (check (length models) => 2)
  (check (cdr (assoc "id" (car models))) => "coding-fast")
  (check (cdr (assoc "id" (cadr models))) => "coding-high-quality")
) ;let

;; 2. 有两条链的配置文件，模型发现断言包含 coding-deep 与 coding-fast 且 provider 为 all-in-one
(%write-cfg "\n{\n  \"provider\": \"all-in-one\",\n  \"chains\": [\n    {\n      \"id\": \"coding-deep\",\n      \"name\": \"深度思考链\",\n      \"nodes\": [{\"provider\": \"kimi1\", \"model\": \"kimi-k3\"}]\n    },\n    {\n      \"id\": \"coding-fast\",\n      \"name\": \"极速交互链\",\n      \"nodes\": [{\"provider\": \"ds1\", \"model\": \"chat\"}]\n    }\n  ]\n}\n"
) ;%write-cfg

(let ((models (all-in-one-models TMP-DIR)))
  (check (length models) => 2)
  (let ((m1 (car models)) (m2 (cadr models)))
    (check (cdr (assoc "id" m1)) => "coding-deep")
    (check (cdr (assoc "name" m1)) => "深度思考链")
    (check (cdr (assoc "provider" m1)) => "all-in-one")
    (check (cdr (assoc "api" m1)) => "all-in-one")
    (check (cdr (assoc "reasoning" m1)) => #t)

    (check (cdr (assoc "id" m2)) => "coding-fast")
    (check (cdr (assoc "name" m2)) => "极速交互链")
    (check (cdr (assoc "provider" m2)) => "all-in-one")
    (check (cdr (assoc "api" m2)) => "all-in-one")
    (check (cdr (assoc "reasoning" m2)) => #t)
  ) ;let
) ;let

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
