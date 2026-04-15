(import (liii check)
  (liii hashlib)
  (liii path)
) ;import


(check-set-mode! 'report-failed)


;; sha256-by-file
;; 计算文件的 SHA256 哈希值。
;;
;; 语法
;; ----
;; (sha256-by-file path)
;;
;; 参数
;; ----
;; path : string?
;; 要计算哈希的文件路径。
;;
;; 返回值
;; -----
;; string?
;; 返回 64 位小写十六进制字符串表示的 SHA256 哈希值。
;;
;; 说明
;; ----
;; 1. 读取文件内容并计算其 SHA256 哈希值
;; 2. 适用于大文件，无需一次性将整个文件加载到内存
;; 3. 推荐用于文件完整性校验和安全敏感场景


;; ; 基本功能测试：文件哈希与字符串哈希一致
(let ((tmp-file "tests/resources/hashlib-test-temp.txt"
      ) ;tmp-file
      (content "hello")
     ) ;
  (path-write-text tmp-file content)
  (check (sha256-by-file tmp-file)
    =>
    (sha256 content)
  ) ;check
  (delete-file tmp-file)
) ;let


;; ; 边界测试：空文件
(let ((tmp-file "tests/resources/hashlib-test-temp.txt"
      ) ;tmp-file
     ) ;
  (path-write-text tmp-file "")
  (check (sha256-by-file tmp-file)
    =>
    (sha256 "")
  ) ;check
  (delete-file tmp-file)
) ;let


;; ; 大文件测试：100MB 文件
(let* ((large-file "tests/resources/hashlib-test-large-local.txt"
       ) ;large-file
       (large-size (* 100 1024 1024))
       (large-content (make-string large-size #\A)
       ) ;large-content
      ) ;
  (path-write-text large-file
    large-content
  ) ;path-write-text
  (check (path-getsize large-file)
    =>
    large-size
  ) ;check
  (check (sha256-by-file large-file)
    =>
    (sha256 large-content)
  ) ;check
  (when (path-exists? large-file)
    (delete-file large-file)
  ) ;when
) ;let*



(check-report)
