;; (liii goldfmt-cache) 模块测试文件
;;
;; 测试格式化缓存的命中/写入/路径计算逻辑。

(import (liii check) (liii base) (liii os) (liii path) (liii goldfmt-cache))

(check-set-mode! 'report-failed)

;; 缓存根目录应位于 ~/.cache/goldfish/fmt/<version> 之下。
(check (path-contains? (path-join (path-home) ".cache" "goldfish" "fmt")
         (path (fmt-cache-base-dir))
       ) ;path-contains?
  =>
  #t
) ;check

;; 创建一个临时文件，验证路径计算、touch 后缓存命中。
(let* ((tmp-dir (path->string (path-join (path (getcwd)) "tools" "fmt" "tests" "liii"))
       ) ;tmp-dir
       (tmp-file (path->string (path-join (path tmp-dir) "goldfmt-cache-tmp.txt")))
      ) ;
  (path-write-text (path tmp-file) "hello goldfmt-cache")
  (check (fmt-cache-hit? tmp-file) => #f)
  (let ((cache-path (fmt-cache-path tmp-file)))
    (check (file-exists? cache-path) => #f)
    (fmt-cache-touch tmp-file)
    (check (file-exists? cache-path) => #t)
    (check (fmt-cache-hit? tmp-file) => #t)
    (when (file-exists? cache-path)
      (delete-file cache-path)
    ) ;when
  ) ;let
  (when (file-exists? tmp-file)
    (delete-file tmp-file)
  ) ;when
) ;let*

(check-report)
