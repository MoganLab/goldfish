(import (liii check)
        (liii base)
        (liii error)
        (liii path)
        (liii time)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; file->njson
;; 从文件读取严格 JSON 文本并解析为 njson 句柄。
;;
;; 语法
;; ----
;; (file->njson path)
;;
;; 参数
;; ----
;; path : string
;; 文件路径。
;;
;; 返回值
;; ----
;; njson-handle
;; 解析成功后返回句柄。
;;
;; 注意
;; ----
;; 读取到的文本必须是合法 JSON。
;;
;; 错误处理
;; ----
;; type-error
;; path 不是字符串时抛出。
;; parse-error
;; 文件内容不是合法 JSON 时抛出。

(define file-to-njson-path
  (path->string (path-join (path-temp-dir)
                           (string-append "goldfish-njson-file-to-"
                                          (number->string (current-jiffy))
                                          ".json")
                           ) ;string-append
  ) ;path->string
) ;define

(path-write-text file-to-njson-path "{\"name\":\"Goldfish\",\"active\":true,\"nums\":[1,2,3]}")

(let-njson ((loaded (file->njson file-to-njson-path)))
  (check (njson-ref loaded "name") => "Goldfish")
  (check (njson-ref loaded "active") => #t)
  (check (njson-ref loaded "nums" 2) => 3)
) ;let-njson

(path-write-text file-to-njson-path "{bad:1}")
(check-catch 'parse-error (file->njson file-to-njson-path))
(check-catch 'type-error (file->njson 1))

(check-report)
