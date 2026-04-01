;; (liii os) 模块函数分类索引
;;
;; os 提供操作系统信息、环境变量、目录遍历、命令执行和文件系统基础操作。
;; 它的接口风格接近 Python `os`，适合脚本化任务和系统集成场景。

;; ==== 常见用法示例 ====
(import (liii os))

;; 示例1：读取当前平台信息
(os-type) ; => "Linux" / "Darwin" / "Windows"

;; 示例2：获取系统临时目录
(os-temp-dir) ; => 临时目录路径

;; 示例3：读取路径分隔符
(pathsep) ; => #\: 或 #\;

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/os "os-type"
;;   bin/gf doc liii/os "listdir"

;; ==== 函数分类索引 ====

;; 一、平台与路径信息
;; 用于读取操作系统元信息的函数
;;   os-arch        - 获取 CPU 架构
;;   os-type        - 获取操作系统类型
;;   os-windows?    - 判断是否为 Windows
;;   os-linux?      - 判断是否为 Linux
;;   os-macos?      - 判断是否为 macOS
;;   os-temp-dir    - 获取系统临时目录
;;   os-sep         - 获取目录分隔符
;;   pathsep        - 获取路径列表分隔符

;; 二、命令与进程
;; 用于执行系统命令和读取进程信息的函数
;;   os-call        - 调用系统命令
;;   getpid         - 获取当前进程 ID
;;   getlogin       - 获取当前登录用户

;; 三、环境变量
;; 用于读取和修改环境变量的函数
;;   getenv         - 读取环境变量
;;   putenv         - 设置环境变量
;;   unsetenv       - 删除环境变量

;; 四、目录与文件系统
;; 用于处理目录、文件和工作路径的函数
;;   mkdir          - 创建目录
;;   chdir          - 切换工作目录
;;   rmdir          - 删除目录
;;   remove         - 删除文件
;;   rename         - 重命名文件或目录
;;   getcwd         - 获取当前工作目录
;;   listdir        - 列出目录内容
;;   access         - 判断路径访问权限
