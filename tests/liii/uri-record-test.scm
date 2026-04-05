;; (liii uri-record) 模块函数分类索引
;;
;; uri-record 模块定义 URI 记录类型和所有访问器函数。
;; 这是 (liii uri) 的基础模块，提供底层的 URI 数据表示。

;; ==== 常见用法示例 ====
(import (liii uri-record))

;; 示例1：创建原始 URI 记录
(define u (make-uri-raw "https" "user@example.com:8080" "/path" '(("a" . "1")) "frag"))

;; 示例2：访问各个字段
(uri-scheme-raw u)   ; => "https"
(uri-netloc-raw u)   ; => "user@example.com:8080"
(uri-path-raw u)     ; => "/path"
(uri-query-raw u)    ; => '(("a" . "1"))
(uri-fragment-raw u) ; => "frag"

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/uri-record "make-uri-raw"
;;   bin/gf doc liii/uri-record "uri-host"

;; ==== 函数分类索引 ====
;;
;; 一、记录类型构造函数与谓词
;;   make-uri-raw      - 从组件创建原始 URI 记录
;;   uri?              - 检查是否为 URI 记录
;;
;; 二、原始字段访问器（由 define-record-type 生成）
;;   uri-scheme-raw    - 获取/设置 scheme 原始值
;;   uri-netloc-raw    - 获取/设置 netloc 原始值
;;   uri-path-raw      - 获取/设置 path 原始值
;;   uri-query-raw     - 获取/设置 query 原始值（alist 格式）
;;   uri-fragment-raw  - 获取/设置 fragment 原始值
;;
;; 三、Scheme/Host/Port 访问器
;;   uri-scheme        - 获取 scheme
;;   uri-raw-scheme    - 获取 scheme（原始值别名）
;;   uri-host          - 从 netloc 解析获取 host
;;   uri-raw-host      - 获取 host（原始值别名）
;;   uri-port          - 获取端口（优先显式端口，否则默认端口）
;;   uri-explicit-port - 获取显式指定的端口
;;   uri-default-port  - 获取 scheme 的默认端口
;;
;; 四、Authority 访问器
;;   uri-user          - 从 netloc 解析获取 user
;;   uri-raw-user      - 获取 user（原始值别名）
;;   uri-password      - 从 netloc 解析获取 password
;;   uri-raw-password  - 获取 password（原始值别名）
;;   uri-authority     - 获取完整 authority（netloc）
;;   uri-raw-authority - 获取 authority（原始值别名）
;;
;; 五、Path 访问器
;;   uri-path          - 获取 path
;;   uri-raw-path      - 获取 path（原始值别名）
;;   uri-path->list    - 将 path 分割为段列表
;;
;; 六、Query 访问器
;;   uri-query         - 获取 query（alist 格式）
;;   uri-query-string  - 将 query alist 转为字符串
;;   uri-query-ref     - 获取指定 key 的 query 值
;;   uri-query-ref*    - 获取 query 值，可指定默认值
;;
;; 七、Fragment 访问器
;;   uri-fragment      - 获取 fragment
;;   uri-raw-fragment  - 获取 fragment（原始值别名）
;;
;; 八、路径相关访问器
;;   uri-parent        - 获取父目录路径
;;   uri-name          - 获取文件名/最后一级
;;   uri-suffix        - 获取文件后缀
;;   uri-suffixes      - 获取所有后缀列表
;;
;; 九、编码/解码函数
;;   uri-encode        - 百分比编码
;;   uri-decode        - 百分比解码
;;   uri-encode-path   - 路径编码（保留斜杠）
;;   uri-decode-path   - 路径解码
;;
;; 十、查询字符串处理
;;   query-string->alist - 将查询字符串解析为 alist
;;   alist->query-string - 将 alist 转为查询字符串
;;
;; 十一、错误处理
;;   uri-error         - 抛出 URI 错误
;;
;; 十二、辅助函数
;;   parse-netloc      - 解析 netloc 字符串
;;   build-netloc      - 从组件构建 netloc 字符串
