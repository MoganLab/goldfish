;; get-write-json
;; 把 JSON 对象格式化写入文件，自动创建父目录。
;;
;; 语法
;; ----
;; (get-write-json p obj)
;;
;; 参数
;; ----
;; p : path?
;; 目标文件路径。
;;
;; obj : json-object?
;; 待写入对象。
;;
;; 返回值
;; ----
;; unspecified
;; 写入文件；读回内容与原对象一致。

(import (liii check) (liii json) (liii os) (liii path) (liii shutil)
  (liii gfproject get store)
) ;import

(check-set-mode! 'report-failed)

(define dir (path-join (path-temp-dir)
              (string-append "gf-get-write-json-" (number->string (getpid)))
            ) ;path-join
) ;define

;; 父目录不存在时自动创建
(let ((p (path-join dir "sub" "out.json"))
      (obj (string->json "{\"a\":1,\"b\":{\"c\":2}}"))
     ) ;
  (get-write-json p obj)
  (check-true (path-file? p))
  (check (get-read-json p) => obj)
) ;let

(rmtree (path->string dir) #t)

(check-report)
