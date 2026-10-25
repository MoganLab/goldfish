;; (liii fs-watch) 模块函数分类索引
;;
;; fs-watch 提供跨平台目录监听（Windows ReadDirectoryChangesW / Linux inotify /
;; macOS kqueue），API 形态对齐 Python watchdog。

;; ==== 常见用法示例 ====
(import (liii fs-watch) (liii go))

;; 示例1：监听目录变更，回调经 channel 桥接回主线程
;; (let* ((ch (make-chan 32))
;;        (handler (make-fs-event-handler
;;                   :on-modified (lambda (ctx ev)
;;                                  (chan-send! ctx (fs-event-path ev) 5000))))
;;        (obs (make-fs-observer)))
;;   (fs-observer-schedule! obs handler "path/to/.git" :context ch)
;;   (fs-observer-start! obs)
;;   ;; 主线程: (chan-try-recv! ch #f) 取回变更路径
;;   ...
;;   (fs-observer-stop! obs))

;; 示例2：四类事件回调
;; (make-fs-event-handler
;;   :on-modified (lambda (ctx ev) ...)
;;   :on-created  (lambda (ctx ev) ...)
;;   :on-deleted  (lambda (ctx ev) ...)
;;   :on-moved    (lambda (ctx ev) (fs-event-dest-path ev)))

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/fs-watch "make-fs-event-handler"
;;   bin/gf doc liii/fs-watch "make-observer"
;;   bin/gf doc liii/fs-watch "fs-observer-schedule!"
;;   bin/gf doc liii/fs-watch "fs-observer-start!"
;;   bin/gf doc liii/fs-watch "fs-observer-stop!"
;;   bin/gf doc liii/fs-watch "fs-watch-dispatch!"

;; ==== 函数分类索引 ====
;;   make-fs-event-handler  - 创建事件处理器（四个回调槽，对齐 FileSystemEventHandler）
;;   make-fs-event          - 构造事件记录（对齐 FileSystemEvent 可实例化）
;;   fs-event?              - 事件记录谓词（含 type/path/dest-path 访问器）
;;   make-fs-observer          - 创建监听 observer（对齐 watchdog Observer）
;;   fs-observer-schedule!     - 注册监听项（目录 + 处理器 + :context）
;;   fs-observer-start!        - 启动监听（后台 go 泵线程分发）
;;   fs-observer-stop!         - 停止监听（幂等，可复用 observer）
;;   fs-watch-dispatch!     - 按事件类型分发回调（纯逻辑核心）
;;   fs-watch-handler->src-list - 处理器回调序列化为源码串列表
;;   fs-watch-start%        - 带校验的底层 watcher 启动包装

;; ==== 线程语义（重要） ====
;; 回调在后台 (liii go) worker 会话执行，不在主会话（对齐 watchdog 回调
;; 运行于 observer 线程）；跨线程状态经 :context 显式传递可序列化值
;; （典型为 channel，等价 Python 回调持有 queue.Queue 的模式）；回调体内
;; 引用的自由符号必须是库导出绑定，不能闭包捕获主会话对象。
