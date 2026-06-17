(import (liii check) (liii path) (liii error) (liii os))

(check-set-mode! 'report-failed)

;; path-write-bytes
;; 写入二进制文件内容。
;;
;; 语法
;; ----
;; (path-write-bytes path data) → unspecified
;;
;; 参数
;; ----
;; path : string | path-value
;; data : bytevector
;;
;; 返回值
;; -----
;; unspecified
;;
;; 错误处理
;; ----
;; type-error 当 data 不是 bytevector 时。

;; 基本写入测试
(let ((write-file (path-join (path-temp-dir) "path-write-bytes-basic.bin")))
  (when (path-exists? write-file)
    (delete-file (path->string write-file))
  ) ;when
  (path-write-bytes write-file (bytevector 65 66 67))
  ;; 读回应为相同字节
  (let ((data (path-read-bytes write-file)))
    (check (bytevector-length data) => 3)
    (check (bytevector-u8-ref data 0) => 65)
    (check (bytevector-u8-ref data 2) => 67)
  ) ;let
  (delete-file (path->string write-file))
) ;let

;; 覆盖写入
(let ((write-file (path-join (path-temp-dir) "path-write-bytes-overwrite.bin")))
  (when (path-exists? write-file)
    (delete-file (path->string write-file))
  ) ;when
  (path-write-bytes write-file (bytevector 1 2 3 4 5))
  (path-write-bytes write-file (bytevector 9 9))
  (let ((data (path-read-bytes write-file)))
    (check (bytevector-length data) => 2)
    (check (bytevector-u8-ref data 0) => 9)
  ) ;let
  (delete-file (path->string write-file))
) ;let

;; 错误处理:data 非 bytevector
(check-catch 'type-error
  (path-write-bytes (path-join (path-temp-dir) "x.bin") "not bytes")
) ;check-catch

(check-report)
