# (liii goldcheck unit-test source parse) 模块函数分类索引

源码库结构解析与导出符号提取，包含库声明、导入导出表及内部定义解析。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、文件路径与库标识转换
- `resolve-repo-file` - 将相对路径解析为相对于仓库根目录的完整路径
- `lib-spec->relative-source-path` - 将库名规范转换为相对源码文件路径

### 二、语法表单与内部定义提取
- `extract-form-definitions` - 从单一顶层表单中提取定义的标识符名称
- `parse-library-decl-definitions` - 从库声明（如 begin、include）中提取定义的标识符

### 三、导入导出与库结构解析
- `extract-decl-export-entries` - 从 export 声明中提取外部名称与内部名称的点对列表
- `extract-decl-imports` - 从 import 声明中提取导入的库规范列表
- `parse-library-structure` - 解析库定义结构，返回 exports、imports 和 defs 的结构化数据
