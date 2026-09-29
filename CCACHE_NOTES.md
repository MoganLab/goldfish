# Library cache

## 布局与失效

- 缓存按运行时指纹隔离；格式版本定义在 `goldfish/core/gfo.scm`。
- 程序缓存：key 为源路径哈希，按源文件 `mtime+size` 失效。
- 库缓存：记录 `(name exports imports bindings macros defs)`；命中要求
  源文件存在且 `mtime+size` 匹配。
- 缓存不替代源码：源缺失或改动一律重展开。

## 内容

- 值 binding 存为顶层或 primitive 引用；宏存为 syntax spec，命中时重放
  定义以恢复绑定。
- 恢复库缓存前先恢复依赖，再求值定义。

## 已知限制

- Macro transformer closure 不可序列化；相关定义通过 syntax spec 重建。
