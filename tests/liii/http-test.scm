;; (liii http) 模块函数分类索引
;;
;; liii http 提供同步、流式和异步三种 HTTP 客户端 API，基于 cpr 库实现。
;; 支持 GET/POST/HEAD 请求；其中 http-get 支持通过 :output-file 或 :stream 流式下载，
;; http-post 支持通过 :files 上传文件。

;; ==== 常见用法示例 ====
(import (liii http)
        (liii json)
) ;import

;; 示例1：同步 GET 请求
;; (let ((r (http-get "https://api.example.com/data")))
;;   (display (r 'status-code))  ; 200
;;   (display (r 'text)))         ; 响应体

;; 示例1.1：直接下载到本地文件
;; (http-get "https://example.com/archive.tar.gz"
;;           :output-file "/tmp/archive.tar.gz")

;; 示例1.2：流式处理下载内容
;; (http-get "https://example.com/events"
;;           :stream #t
;;           :callback (lambda (chunk userdata)
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
;;   http-get             - 发送 GET 请求，也支持通过 :output-file / :stream 流式下载
;;   http-post            - 发送 POST 请求，也支持通过 :files 上传文件
;;
;; 二、响应处理
;;   http-ok?     - 检查响应是否成功（2xx状态码）
;;
;; 三、流式 HTTP 请求
;;   http-stream-get   - 流式 GET，通过回调处理数据块
;;   http-stream-post  - 流式 POST，通过回调处理数据块
;;
;; 四、异步 HTTP 请求
;;   http-async-get    - 异步 GET 请求
;;   http-async-post   - 异步 POST 请求
;;   http-async-head   - 异步 HEAD 请求
;;   http-poll         - 轮询并执行已完成的异步请求回调
;;   http-wait-all     - 等待所有异步请求完成
