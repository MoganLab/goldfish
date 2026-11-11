;; all-in-one-provider-entry
;; 构造元供应商的完整目录条目。
;;
;; 语法
;; ----
;; (all-in-one-provider-entry . opt-config-dir)
;;
;; 参数
;; ----
;; opt-config-dir : (or string path #f) (可选)
;; 可选配置目录，未提供或为 #f 时使用默认的 ~/.gf/agent/all-in-one.json。
;;
;; 返回值
;; ----
;; pair
;; 目录条目 ("all-in-one" ("name" . "all-in-one") ("api" . "all-in-one") ("models" ...))。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii os)
  (liii pi all_in_one provider)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-entry-test-" (number->string (getpid)))
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

(%write-cfg "\n{\n  \"provider\": \"all-in-one\",\n  \"chains\": [\n    {\"id\": \"c1\", \"nodes\": [{\"provider\": \"p\", \"model\": \"m\"}]}\n  ]\n}\n"
) ;%write-cfg

(let ((entry (all-in-one-provider-entry TMP-DIR)))
  (check (car entry) => "all-in-one")
  (let ((fields (cdr entry)))
    (check (cdr (assoc "name" fields)) => "all-in-one")
    (check (cdr (assoc "api" fields)) => "all-in-one")
    (let ((models (cdr (assoc "models" fields))))
      (check (length models) => 1)
      (check (cdr (assoc "id" (car models))) => "c1")
    ) ;let
  ) ;let
) ;let

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
