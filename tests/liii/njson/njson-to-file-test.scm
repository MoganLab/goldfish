(import (liii check)
        (liii base)
        (liii error)
        (liii path)
        (liii time)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson->file
;; 将 njson 值写入文件，输出为 pretty JSON 文本。
;;
;; 语法
;; ----
;; (njson->file path value)
;;
;; 参数
;; ----
;; path : string
;; 文件路径。
;; value : njson-handle | strict json scalar
;; 待写入的 JSON 值。
;;
;; 返回值
;; ----
;; integer?
;; 写入字节数。
;;
;; 注意
;; ----
;; 输出内容会按 pretty 格式写入。
;;
;; 错误处理
;; ----
;; type-error
;; path 不是字符串或 value 不可序列化时抛出。

(define njson-to-file-path
  (path->string (path-join (path-temp-dir)
                           (string-append "goldfish-njson-to-file-"
                                          (number->string (current-jiffy))
                                          ".json")
                           ) ;string-append
  ) ;path->string
) ;define

(let-njson ((root (string->njson "{\"b\":1,\"a\":2}")))
  (check-true (> (njson->file njson-to-file-path root) 0))
  (check (path-read-text njson-to-file-path)
         => "{\n  \"a\": 2,\n  \"b\": 1\n}"
  ) ;check
) ;let-njson

(check-true (> (njson->file njson-to-file-path 'null) 0))
(let-njson ((loaded-null (file->njson njson-to-file-path)))
  (check-true (njson-null? loaded-null))
) ;let-njson

(check-catch 'type-error (njson->file 1 'null))
(check-catch 'type-error (njson->file njson-to-file-path 'foo))

(define njson-to-file-freed (string->njson "{\"k\":1}"))
(check-true (njson-free njson-to-file-freed))
(check-catch 'type-error (njson->file njson-to-file-path njson-to-file-freed))

(check-report)
