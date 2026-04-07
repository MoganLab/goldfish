;; (liii http) 模块函数分类索引
;;
;; liii http 提供同步、流式和异步三种 HTTP 客户端 API，基于 cpr 库实现。
;; 支持 GET/POST/HEAD/multipart 请求，可设置参数、请求头和代理。

;; ==== 常见用法示例 ====
(import (liii http)
        (liii json)
) ;import

;; 示例1：同步 GET 请求
;; (let ((r (http-get "https://api.example.com/data")))
;;   (display (r 'status-code))  ; 200
;;   (display (r 'text)))         ; 响应体

;; 示例2：带查询参数的 POST 请求
;; (http-post "https://api.example.com/submit"
;;            :params '(("key" . "value"))
;;            :data "{\"name\":\"test\"}"
;;            :headers '(("Content-Type" . "application/json")))

;; 示例3：multipart 文件上传
;; (http-multipart-post "https://api.example.com/upload"
;;   '(((name . "meta") (value . "release"))
;;     ((name . "artifact") (file . "./dist/app.tar.gz")
;;      (filename . "app.tar.gz")
;;      (content-type . "application/gzip"))))

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
;;   http-get             - 发送 GET 请求
;;   http-post            - 发送 POST 请求
;;   http-multipart-post  - 发送 multipart/form-data POST 请求
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
