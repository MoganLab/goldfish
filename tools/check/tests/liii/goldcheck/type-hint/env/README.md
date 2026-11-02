# (liii goldcheck type-hint env) 模块函数分类索引

类型环境加载与依赖分析，支持沿原库的 import 和再导出依赖关系穿透查找过程签名，构建静态类型环境并评估覆盖率。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、Hint 根与文件定位
- `hint-roots-of` - 解析 hint 文件所属的本地与全局 hint 查找根路径
- `library-file` - 在指定根目录下查找库的源文件或 hint 文件路径
- `hint-lib-file` - 获取指定库符号对应的 hint 库文件路径

### 二、库信息缓存与依赖闭包
- `cached-library-info` - 带缓存的库 AST 解析信息获取
- `source-closure` - 按广度优先遍历原库及其全部 import 构成的依赖闭包
- `defining-libs` - 从依赖闭包中筛选本地定义了指定符号的库
- `exporting-libs` - 从依赖闭包中筛选导出了指定符号的库

### 三、类型环境构建与覆盖率评估
- `build-env` - 为目标库构建包含自身签名、依赖项穿透及基础内建签名的类型环境
- `full-coverage?` - 判断目标原库导出的所有符号是否在类型环境中完全覆盖
