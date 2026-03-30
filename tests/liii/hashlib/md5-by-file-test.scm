(import (liii check)
        (liii hashlib)
        (liii path)
) ;import

(check-set-mode! 'report-failed)

;; md5-by-file
;; 计算文件的 MD5 哈希值。
;;
;; 语法
;; ----
;; (md5-by-file path)
;;
;; 参数
;; ----
;; path : string?
;; 要计算哈希的文件路径。
;;
;; 返回值
;; -----
;; string?
;; 返回 32 位小写十六进制字符串表示的 MD5 哈希值。
;;
;; 说明
;; ----
;; 1. 读取文件内容并计算其 MD5 哈希值
;; 2. 适用于大文件，无需一次性将整个文件加载到内存
;; 3. 常用于文件完整性校验

;;; 基本功能测试：文件哈希与字符串哈希一致
(let ((tmp-file "tests/resources/hashlib-test-temp.txt")
      (content "hello"))
  (path-write-text tmp-file content)
  (check (md5-by-file tmp-file) => (md5 content))
  (delete-file tmp-file)
) ;let

;;; 边界测试：空文件
(let ((tmp-file "tests/resources/hashlib-test-temp.txt"))
  (path-write-text tmp-file "")
  (check (md5-by-file tmp-file) => (md5 ""))
  (delete-file tmp-file)
) ;let


(check-report)
