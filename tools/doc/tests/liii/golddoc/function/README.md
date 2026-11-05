# (liii golddoc function) 模块导览

`(liii golddoc function)` 提供函数级文档的定位能力：根据库查询字符串与导出名，
找到承载该函数文档的叶子测试文件，并汇总某个库中已经编写文档的函数名。

使用场景：

- `gf doc` 在按库浏览文档时，需要列出某个库已经文档化的函数
- 按函数名反查文档路径时，需要将库查询与导出名映射到叶子测试文件
- CLI 需要给出 "该函数尚未建立文档" 之类的精确提示

## 查看函数文档

```bash
# 查看本模块导览
bin/gf doc liii/golddoc/function

# 查看具体函数的文档
bin/gf doc liii/golddoc/function function-doc-path
```

## 函数分类索引

### 文档定位

- `function-doc-path(library-query exported-name)` : 根据库查询字符串和导出名，定位对应的叶子测试文档文件，返回路径字符串或 `#f`。

### 文档汇总

- `library-documented-functions(library-query)` : 扫描库对应的测试目录，返回已建立文档注释的函数名列表。

### 命名映射（复用自 `(liii golddoc naming)`）

- `exported-name->test-stem(exported-name)` : 将导出名转换为叶子测试文件 stem，是文档定位的前置步骤。
