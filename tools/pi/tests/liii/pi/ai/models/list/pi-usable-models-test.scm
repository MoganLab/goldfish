(import (scheme base)
  (scheme file)
  (srfi srfi-1)
  (liii check)
  (liii list)
  (liii sort)
  (liii path)
  (liii shutil)
  (liii pi ai auth)
  (liii pi ai models catalog)
  (liii pi ai models list)
  (liii pi ai models)
  (liii pi code config user-catalog)
) ;import

(check-set-mode! 'report-failed)

(define AGENT-DIR (path->string (path-join (path-temp-dir) "pi-usable-models-test")))

(when (file-exists? AGENT-DIR)
  (rmtree AGENT-DIR)
) ;when

(path-mkdir AGENT-DIR :parents #t :exist-ok #t)
(path-write-text (path->string (path-join (path AGENT-DIR) (path "auth.json")))
  "{\"google\":{\"type\":\"api_key\",\"key\":\"fixture-google-key\"},\"deepseek\":{\"type\":\"api_key\",\"key\":\"fixture-deepseek-key\"}}"
) ;path-write-text

(let* ((store (pi-build-user-catalog (path->string AGENT-DIR)))
       (auth (pi-load-auth AGENT-DIR))
       ;; 传入非空 env 表以隔离真实环境变量，避免开发机上的密钥影响结果
       (env (list (cons "HOME" AGENT-DIR)))
       (models (pi-usable-models store auth env))
       ;; 便于断言的派生视图
       (providers (map (lambda (m) (cdr (assoc 'provider m))) models))
       (keys (map (lambda (m)
                    (string-append (cdr (assoc 'provider m)) "/" (cdr (assoc 'id m))))
                  models))
       (gemini-pro (find (lambda (m) (equal? (cdr (assoc 'id m)) "gemini-2.5-pro")) models))
      ) ;

  ;; 1. 只包含有凭证 provider（google/deepseek）的模型
  (check-true (pair? models))
  (check-true (and (member "google" providers) #t))
  (check-true (and (member "deepseek" providers) #t))
  (check-false (member "minimax" providers))

  ;; 2. 条目字段完整：provider/id/name/reasoning 齐备
  (check-true (pair? gemini-pro))
  (check (cdr (assoc 'provider gemini-pro)) => "google")
  (check (cdr (assoc 'name gemini-pro)) => "Gemini 2.5 Pro")
  (check (cdr (assoc 'reasoning gemini-pro)) => #t)

  ;; 3. 排序：provider 字典序，同 provider 内 id 字典序
  (check (length keys) => (length (delete-duplicates keys)))
  (check keys => (list-sort string<? keys))

  ;; 4. 无凭证时返回空表
  (check (pi-usable-models store '() env) => '())

) ;let*

(rmtree AGENT-DIR)

(check-report)
