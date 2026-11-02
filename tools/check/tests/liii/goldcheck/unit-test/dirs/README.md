# (liii goldcheck unit-test dirs) 模块函数分类索引

`(liii goldcheck unit-test dirs)` 提供了单元测试目录级规范检查功能，
负责检查测试目录及源码对应测试目录下的 README.md 存在性、废弃单测检测以及导出函数测试覆盖。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/goldcheck/unit-test/dirs "函数名"
```

## 函数分类索引

### 一、测试目录规范检查
- `check-unit-test-dirs` - 检查测试文件或源码文件所属测试目录的规范与测试完整性
