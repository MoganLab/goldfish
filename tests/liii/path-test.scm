;; (liii path) 模块函数分类索引
;;
;; path 是一个跨平台的路径抽象，支持 POSIX 和 Windows 两种路径格式。
;; 与直接拼接字符串相比，它更适合表达路径层级、父目录和文件系统操作。


;; ==== 常见用法示例 ====
(import (liii path))


;; 示例1：创建路径并获取信息
(define p (path "/home/user/file.txt"))
(path->string p)
(path-name p)
(path-stem p)
(path-suffix p)
(path-parent p)


;; 示例2：路径拼接
(path-join "/home/user"
  "Documents"
  "file.txt"
) ;path-join
;; => #<path /home/user/Documents/file.txt>


;; 示例3：检查路径属性
(path-absolute? (path "/usr/bin"))
(path-relative? (path "./file.txt"))
(path-exists? (path "/etc/passwd"))


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/path "path?"
;;   bin/gf doc liii/path "path-join"


;; ==== 函数分类索引 ====


;; 一、构造函数
;; 用于创建 path 对象的函数
;;   path              - 从字符串或其他 path 创建路径对象
;;   path-from-string  - 从字符串创建路径（path 的别名）
;;   path-from-parts   - 从路径各部分（向量）创建路径
;;   path-of-drive     - 创建 Windows 驱动器路径
;;   path-root         - 创建根路径 "/"
;;   path-cwd          - 获取当前工作目录路径
;;   path-home         - 获取用户主目录路径
;;   path-temp-dir     - 获取临时目录路径
;;   path-from-env     - 从环境变量创建路径


;; 二、谓词函数
;; 用于判断路径类型和属性的函数
;;   path?             - 判断是否为 path 对象
;;   path-absolute?    - 判断是否为绝对路径
;;   path-relative?    - 判断是否为相对路径
;;   path-exists?      - 判断路径是否存在
;;   path-dir?         - 判断是否为目录
;;   path-file?        - 判断是否为文件
;;   path-equals?      - 判断两个路径是否相等
;;   path=?            - path-equals? 的别名


;; 三、属性访问
;; 用于获取路径各组成部分的函数
;;   path-parts        - 获取路径各部分（向量）
;;   path-type         - 获取路径类型（'posix 或 'windows）
;;   path-drive        - 获取 Windows 驱动器字母
;;   path-name         - 获取文件名（最后一部分）
;;   path-stem         - 获取文件名（不含扩展名）
;;   path-suffix       - 获取文件扩展名


;; 四、路径操作
;; 用于操作和转换路径的函数
;;   path-copy         - 复制 path 对象
;;   path-join         - 拼接多个路径片段
;;   path-parent       - 获取父目录路径
;;   path->string      - 将路径转换为字符串


;; 五、目录列表操作
;; 用于列出目录内容的函数
;;   path-list         - 列出目录中的文件名（返回向量）
;;   path-list-path    - 列出目录中的路径对象（返回向量）


;; 六、文件系统操作
;; 用于操作文件系统的函数
;;   path-read-text    - 读取文件内容为文本
;;   path-read-bytes   - 读取文件内容为字节向量
;;   path-write-text   - 将文本写入文件
;;   path-append-text  - 追加文本到文件
;;   path-touch        - 创建空文件或更新时间戳
;;   path-getsize      - 获取文件大小
;;   path-rmdir        - 删除空目录
;;   path-unlink       - 删除文件
