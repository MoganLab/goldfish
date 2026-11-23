;; get-read-json
;; 读取 JSON 文件为对象；不存在或非法时返回空对象。
;;
;; 语法
;; ----
;; (get-read-json p)
;;
;; 参数
;; ----
;; p : path? or string?
;; JSON 文件路径。
;;
;; 返回值
;; ----
;; json-object?
;; 解析结果；无法读取时为 '()。

(import (liii check) (liii json) (liii os) (liii path) (liii shutil)
  (liii gfproject get store)
) ;import

(check-set-mode! 'report-failed)

(define dir (path-join (path-temp-dir)
              (string-append "gf-get-read-json-" (number->string (getpid)))
            ) ;path-join
) ;define
(path-mkdir dir #t #t)

;; 正常读取
(path-write-text (path-join dir "ok.json") "{\"a\":1}")
(check (json-ref (get-read-json (path-join dir "ok.json")) "a") => 1)

;; 不存在
(check (get-read-json (path-join dir "missing.json")) => '(()))

;; 非法 JSON / 非对象
(path-write-text (path-join dir "bad.json") "not json")
(check (get-read-json (path-join dir "bad.json")) => '(()))
(path-write-text (path-join dir "arr.json") "[1,2]")
(check (get-read-json (path-join dir "arr.json")) => '(()))

(rmtree (path->string dir) #t)

(check-report)
