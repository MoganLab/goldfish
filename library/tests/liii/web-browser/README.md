# (liii web-browser) 模块函数分类索引

`(liii web-browser)` 提供跨平台的默认浏览器打开接口，用于展示 URL 或进行 OAuth2 授权回调等场景。
接口实现参考 Python `webbrowser` 模块，通过参数列表安全调用外部启动器，防止命令注入与参数注入。

## 查看函数文档

```bash
gf doc liii/web-browser
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/web-browser "web-browser-open"
```

## 函数分类索引

### 一、浏览器打开
- `web-browser-open` - 使用默认浏览器打开指定的 URL
- `web-browser-open-new` - 尝试在新的浏览器窗口中打开指定的 URL
- `web-browser-open-new-tab` - 尝试在新的浏览器标签页中打开指定的 URL

### 二、环境探测与命令解析
- `web-browser-available?` - 检查当前环境中是否有可用的浏览器启动命令
- `web-browser-command` - 安全校验 URL 并解析出要执行的命令行参数列表
