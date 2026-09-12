# ccache

## 布局与失效

- 缓存根按管线指纹分段（见 `LAYER.md` L2）；格式版本见 `goldfish/core/gfo.scm`。
- 程序缓存：key 为源路径哈希，按源文件 `mtime+size` 失效。
- 库缓存：记录 `(name exports imports bindings macros defs)`；命中要求
  源文件存在且 `mtime+size` 匹配。
- 缓存是源文件的编译产物，不替代源码：源缺失或改动一律重展开。

## 内容

- 值 binding 净化为 `(toplevel gensym home original exported?)` /
  `(primitive name)`；宏存净化后的 syntax spec（保 scope 卫生），
  命中时经 `expand-lib-define-syntax` 重放重建。
- 重建先 `import-into-library!` 恢复依赖（含 re-export 的宏），
  再 `library-registry-set!`，最后求值 defs（跨库 `module-ref` 先加载）。
- `write-roundtrip`（`goldfish/liii/reader.scm`）保证与 R7RS reader 的
  读写对偶：竖线转义、dotted pair、`#g(tag ...)` record 层、
  共享/循环 `#n=`/`#n#`；遇 closure 明确报错而非静默损坏。

## 已知限制

- closure（macro transformer）不可序列化；含宏定义的程序文件逐 form 回退。
