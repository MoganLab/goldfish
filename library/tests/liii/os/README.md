# (liii os) 模块导览

`(liii os)` 提供操作系统信息、环境变量、目录遍历、命令执行和文件系统基础操作。
它的接口风格接近 Python `os`，适合脚本化任务和系统集成场景。

使用时导入：

```scheme
(import (liii os))
```

## 查看函数文档

使用以下命令查看函数的文档和用例：

```bash
gf doc liii/os "os-type"
gf doc liii/os "listdir"
```

## 函数分类索引

### 一、平台与路径信息

用于读取操作系统元信息的函数：

| 函数 | 说明 |
|---|---|
| `os-arch` | 获取 CPU 架构 |
| `os-type` | 获取操作系统类型 |
| `os-windows?` | 判断是否为 Windows |
| `os-linux?` | 判断是否为 Linux |
| `os-macos?` | 判断是否为 macOS |
| `os-temp-dir` | 获取系统临时目录 |
| `os-sep` | 获取目录分隔符 |
| `pathsep` | 获取路径列表分隔符 |
| `os-terminal-size` | 获取终端窗口尺寸 |

### 二、命令与进程

用于执行系统命令和读取进程信息的函数：

| 函数 | 说明 |
|---|---|
| `os-call` | 调用系统命令 |
| `os-tty?` | 检查文件描述符是否连接到终端 |
| `getpid` | 获取当前进程 ID |
| `getlogin` | 获取当前登录用户 |

### 三、环境变量

用于读取和修改环境变量的函数：

| 函数 | 说明 |
|---|---|
| `getenv` | 读取环境变量 |
| `putenv` | 设置环境变量 |
| `unsetenv` | 删除环境变量 |

### 四、目录与文件系统

用于处理目录、文件和工作路径的函数：

| 函数 | 说明 |
|---|---|
| `mkdir` | 创建目录 |
| `chdir` | 切换工作目录 |
| `rmdir` | 删除空目录 |
| `remove` | 删除文件 |
| `rename` | 重命名文件或目录 |
| `getcwd` | 获取当前工作目录 |
| `listdir` | 列出目录内容 |
| `access` | 判断路径访问权限 |
| `isdir` | 判断路径是否为已存在的目录 |

### 五、控制台原始输入

用于 TUI 场景下的控制台底层按键读取：

| 函数 | 说明 |
|---|---|
| `console-kbhit?` | 非阻塞检测是否有按键可读 |
| `console-getch` | 读取一个按键字节 |
| `console-raw-mode!` | 开启/关闭终端 raw mode |
| `console-raw-mode-state` | 读取终端模式诊断信息 |

### 六、密码学安全随机数

对接操作系统熵源，提供不可预测的随机字节：

| 函数 | 说明 |
|---|---|
| `urandom` | 生成指定字节长度的密码学安全随机 bytevector |
