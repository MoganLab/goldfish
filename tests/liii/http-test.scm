;; (liii http) 模块函数分类索引
;;
;; liii http 提供同步、流式和异步三种 HTTP 客户端 API，基于 cpr 库实现。
;; 支持 GET/POST/HEAD 请求；其中 http-get 在 :stream #t 时可做简单流式下载，
;; http-post 支持通过 :files 上传文件，并在 :stream #t 时按 chunk 处理响应体。


;; ==== 常见用法示例 ====
(import (liii http) (liii json))


;; 示例1：同步 GET 请求
;; (let ((r (http-get "https://api.example.com/data")))
;;   (display (r 'status-code))  ; 200
;;   (display (r 'text)))         ; 响应体


;; 示例1.1：直接下载到本地文件
;; (http-get "https://example.com/archive.tar.gz"
;;           :stream #t
;;           :output-file "/tmp/archive.tar.gz")


;; 示例1.2：通过 :callback 按 chunk 处理下载内容
;; (http-get "https://example.com/events"
;;           :stream #t
;;           :callback (lambda (chunk)
;;                       (display chunk)
;;                       #t))


;; 示例2：带查询参数的 POST 请求
;; (http-post "https://api.example.com/submit"
;;            :params '(("key" . "value"))
;;            :data "{\"name\":\"test\"}"
;;            :headers '(("Content-Type" . "application/json")))


;; 示例3：通过 http-post 上传文件
;; (http-post "https://api.example.com/upload"
;;   :data '(("meta" . "release"))
;;   :files '(("artifact" . ((file . "./dist/app.tar.gz")
;;                           (filename . "app.tar.gz")
;;                           (content-type . "application/gzip")))))


;; 示例3.1：通过 http-post 流式处理响应
;; (http-post "https://api.example.com/export"
;;            :data "{\"job\":\"export\"}"
;;            :headers '(("Content-Type" . "application/json"))
;;            :stream #t
;;            :callback (lambda (chunk)
;;                        (display chunk)
;;                        #t))


;; 示例4：异步并发请求
;; (http-async-get "https://api.example.com/1" callback)
;; (http-async-get "https://api.example.com/2" callback)
;; (http-wait-all 30)  ; 等待所有请求完成，超时30秒



;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/http "function-name"


;; ==== 函数分类索引 ====
;;
;; 一、同步 HTTP 请求
;;   http-head            - 发送 HEAD 请求，获取响应头
;;   http-get             - 发送 GET 请求；:stream #t 时也支持简单流式下载
;;   http-post            - 发送 POST 请求，也支持通过 :files 上传文件
;;
;; 二、响应处理
;;   http-ok?     - 检查响应是否成功（2xx状态码）
;;
;; 三、异步 HTTP 请求
;;   http-async-get    - 异步 GET 请求
;;   http-async-post   - 异步 POST 请求
;;   http-async-head   - 异步 HEAD 请求
;;   http-poll         - 轮询并执行已完成的异步请求回调
;;   http-wait-all     - 等待所有异步请求完成
