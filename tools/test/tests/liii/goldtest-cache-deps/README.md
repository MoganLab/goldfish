# (liii goldtest-cache-deps)

测试缓存的依赖解析与指令分析模块，负责按真实加载路径递归追踪 `.scm` 依赖、启动引导文件、`tests/resources/` 以及测试文件头部的 `@test-no-cache` / `@test-depends:` 指令。

## 接口列表

| 函数 | 说明 |
| --- | --- |
| `extract-file-imports` | 提取 Scheme 源文件中的 `import` 库名列表 |
| `resolve-import-path` | 按真实加载路径将库名解析为 `.scm` 源文件路径 |
| `find-test-dependencies` | 递归收集测试文件、引导文件、`resources/` 及 `@test-depends` 全部依赖 |
| `test-no-cache-directive?` | 检测测试文件头部是否声明了 `; @test-no-cache` |
