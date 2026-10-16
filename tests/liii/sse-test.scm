;; (liii sse) 模块函数分类索引
;;
;; sse 提供 Server-Sent Events (SSE) 事件流的编解码与分帧处理能力。
;; 适合大语言模型 (LLM) 流式调用、实时长连接通知推送与网络数据分片解析。


;; ==== 常见用法示例 ====
(import (liii sse))


;; 示例1：从输入端口读取一个 SSE 事件

(define p (open-input-string "event: chat\ndata: hello\n\n"))

(define ev (read-sse-event p))
(sse-event-event ev)
(sse-event-data ev)


;; 示例2：创建增量 Chunk 解码器消费网络流分片

(define received-data #f)

(define decoder
  (make-sse-decoder (lambda (event) (set! received-data (sse-event-data event))))
) ;define
(decoder "data: hel")
(decoder "lo\n\n")


;; 示例3：序列化事件为 SSE 标准格式文本
(sse-event->string (make-sse-event "update" "new version" "id-1" 3000))


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/sse "read-sse-event"
;;   bin/gf doc liii/sse "make-sse-decoder"


;; ==== 函数分类索引 ====


;; 一、核心记录类型与访问器
;; 用于构造与检查 SSE 事件记录对象的函数
;;   <sse-event>         - SSE 事件记录类型
;;   make-sse-event      - 构造 SSE 事件记录对象
;;   sse-event?          - 判断对象是否为 SSE 事件记录
;;   sse-event-event     - 获取事件类型（默认 "message"）
;;   sse-event-data      - 获取事件负载数据字符串
;;   sse-event-id        - 获取事件 ID 或 #f
;;   sse-event-retry     - 获取重试间隔毫秒数或 #f
;;   sse-event-json      - 将事件负载数据反序列化为 JSON 结构


;; 二、流式读取与增量解码
;; 用于从端口或 Chunk 增量分片消费 SSE 事件流的函数
;;   read-sse-event      - 从输入端口消费并解析下一个完整 SSE 事件
;;   make-sse-decoder    - 构造增量 Chunk 分片解码器闭包


;; 三、事件序列化与写出
;; 用于将 SSE 事件编码输出为标准文本流的函数
;;   sse-event->string   - 将事件记录序列化为 SSE 格式文本字符串
;;   write-sse-event     - 将事件记录序列化并写出到输出端口
