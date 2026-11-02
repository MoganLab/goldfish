# (liii pi code modes interactive run) 模块函数分类索引

`(liii pi code modes interactive run)` 提供了 pi 交互模式的应用运行主循环、在途任务中止与退出刷屏管理。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/pi/code/modes/interactive/run "函数名"
```

## 函数分类索引

### 一、交互循环与退出
- `pi-app-run` - 运行交互式 TUI 应用主事件循环
- `pi-app-abort-inflight!` - 退出或中断时中止在途后台任务并等待结算
- `pi-app-print-transcript!` - 退出全屏备用屏后原子打印对话历史记录到主屏
