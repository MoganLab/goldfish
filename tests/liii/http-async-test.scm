;; (liii http-async) 模块函数分类索引
;;
;; liii http-async 提供异步 HTTP 客户端 API，基于 cpr 库实现。
;; 支持 GET/POST/HEAD 异步请求，通过回调函数处理响应，
;; 并提供 http-poll/http-wait-all 管理异步请求的生命周期。


;; ==== 常见用法示例 ====
(import (liii http-async))


;; 示例1：异步并发请求
;; (http-async-get "https://api.example.com/1" callback)
;; (http-async-get "https://api.example.com/2" callback)
;; (http-wait-all 30)  ; 等待所有请求完成，超时30秒


;; 示例1.1：回调函数接收响应
;; (http-async-get "https://api.example.com/data"
;;   (lambda (r)
;;     (display (r 'status-code))  ; 200
;;     (display (r 'text))         ; 响应体
;;   ) ;lambda
;; ) ;http-async-get


;; 示例2：带数据的异步 POST 请求
;; (http-async-post "https://api.example.com/submit"
;;   callback
;;   :data "{\"name\":\"test\"}"
;;   :headers '(("Content-Type" . "application/json")))


;; 示例3：异步 HEAD 请求
;; (http-async-head "https://api.example.com" callback)


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/http-async "function-name"


;; ==== 函数分类索引 ====
;;
;; 一、异步 HTTP 请求
;;   http-async-get    - 异步 GET 请求
;;   http-async-post   - 异步 POST 请求
;;   http-async-head   - 异步 HEAD 请求
;;
;; 二、异步请求管理
;;   http-poll         - 轮询并执行已完成的异步请求回调
;;   http-wait-all     - 等待所有异步请求完成
;;
;; 三、响应处理
;;   http-ok?     - 检查响应是否成功（2xx状态码）
