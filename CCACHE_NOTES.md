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

## 发行期预编译

冷启动的首次运行在空缓存下会编译整个 runtime 与惰性优化器管线
（数十秒）。发行时可用同一条缓存机制预编译，使新环境的首次运行 ≈ 热启动：

1. 构建 release 二进制后运行 `tools/warm-bootstrap-cache.sh`。它在隔离目录里
   用该二进制编译种子工作流，产出完整缓存，并同时校验自举所需产物
   （`native_bootstrap_artifacts`）与优化器管线产物
   （`native_precompile_artifacts`：`core/ir`、`match*`、`compiler*`、
   `tree-il`）。优化器产物不在启动必需集内（否则每次启动都要重新解析
   大体积编译器产物），但发行缓存必须包含，否则首次编译程序会从源码
   重编优化器。
2. 随发行分发该缓存的内容寻址目录 `v<fingerprint>/`（指纹 = 可执行文件哈希
   + 源哈希）。隔离预热是完整冷启动，会同时捕获 bootstrap installer 的
   bundle（`expander/lib/install.scm-o2.gfo`），随目录整体拷贝分发；新装
   环境的首次启动因此直接走 installer 回放（热路径）。
3. 安装步骤把该目录放到目标用户的 `$XDG_CACHE_HOME/goldfish/native-ccache/`
   （或 `GOLDFISH_CACHE_DIR` 指向处）。目录按指纹命名，不匹配的缓存会被
   忽略并重建，因此分发旧缓存是安全的。

