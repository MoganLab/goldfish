(import (liii check) (liii hashlib) (liii path) (liii os))

;; NOTE: the temp path carries the pid -- sha1/md5/sha256-by-file tests
;; share this directory and run in parallel workers under `gf test --all`.
(define hashlib-tmp-file
  (string-append "tests/resources/hashlib-test-temp-" (number->string (getpid)) ".txt"))


(check-set-mode! 'report-failed)


;; sha1-by-file
;; 计算文件的 SHA1 哈希值。
;;
;; 语法
;; ----
;; (sha1-by-file path)
;;
;; 参数
;; ----
;; path : string?
;; 要计算哈希的文件路径。
;;
;; 返回值
;; -----
;; string?
;; 返回 40 位小写十六进制字符串表示的 SHA1 哈希值。
;;
;; 说明
;; ----
;; 1. 读取文件内容并计算其 SHA1 哈希值
;; 2. 适用于大文件，无需一次性将整个文件加载到内存
;; 3. 常用于文件完整性校验


;; ; 基本功能测试：文件哈希与字符串哈希一致
(let ((tmp-file hashlib-tmp-file) (content "hello"))
  (path-write-text tmp-file content)
  (check (sha1-by-file tmp-file) => (sha1 content))
  (delete-file tmp-file)
) ;let


;; ; 边界测试：空文件
(let ((tmp-file hashlib-tmp-file))
  (path-write-text tmp-file "")
  (check (sha1-by-file tmp-file) => (sha1 ""))
  (delete-file tmp-file)
) ;let



(check-report)
