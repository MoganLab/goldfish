;; liii_string.scm: 演示报错发生在标准库 (liii string) 内部类型检查的完整调用栈

(import (scheme base) (liii string))

;; 业务底层：调用 (liii string) 的 string-remove-suffix

(define (clean-file-extension filename ext)
  (cons "cleaned" (string-remove-suffix filename ext))
) ;define

;; 业务中间层

(define (process-attachment file ext)
  (cons "attachment" (clean-file-extension file ext))
) ;define

;; 入口函数：对文件批次进行处理，故意传入非字符串后缀 42（string-remove-suffix 要求必须是 string）

(define (handle-upload-job filename invalid-ext)
  (cons "job-result" (process-attachment filename invalid-ext))
) ;define

(handle-upload-job "document.pdf" 42)
