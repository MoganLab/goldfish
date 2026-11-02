# (liii goldcheck collect) 模块函数分类索引

gf check 待检测目标文件收集，支持全仓扫描、位置参数路径过滤、Git 增量变更文件解析及多文件后缀筛选。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、文件后缀匹配
- `file-suffix-in?` - 判断文件路径是否以指定后缀列表中的任一后缀结尾

### 二、目录递归收集
- `collect-files-by-suffixes` - 递归扫描指定目录收集匹配后缀的文件相对路径
- `collect-scheme-files` - 递归扫描指定目录收集 Scheme (.scm) 文件
- `collect-positional-files` - 收集位置参数（文件或目录）下的匹配文件
- `collect-repo-files` - 扫描整个代码仓库根目录下的全部 Scheme 文件

### 三、参数目标解析
- `resolve-target-files` - 根据命令行参数（全仓/位置参数/--changed-since）解析目标 Scheme 文件列表
- `resolve-no-crlf-files` - 为 no-crlf 规则解析匹配配置后缀与路径的目标文件列表
