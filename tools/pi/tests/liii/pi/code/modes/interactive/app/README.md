# (liii pi code modes interactive app) 模块函数分类索引

`(liii pi code modes interactive app)` 实现了 Pi 交互式全屏终端应用程序的状态机与界面控制器。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/pi/code/modes/interactive/app "函数名"
```

## 函数分类索引

### 一、构造与判定
- `make-pi-app` - 构造交互式应用实例
- `pi-app?` - 判定对象是否为交互式应用实例

### 二、核心组件与状态访问
- `pi-app-term` - 获取应用关联的终端
- `pi-app-screen` - 获取应用关联的屏幕缓冲区
- `pi-app-editor` - 获取输入编辑器组件
- `pi-app-queue` - 获取任务执行队列
- `pi-app-chat` - 获取对话记录卡片列表
- `pi-app-working` - 获取工作状态指示器
- `pi-app-working-tick!` - 推进工作指示器动画节拍

### 三、渲染与输入处理
- `pi-app-render!` - 渲染完整应用界面并更新光标
- `pi-app-handle-input!` - 处理按键与快捷键输入
- `pi-app-handle-mouse!` - 处理单次鼠标事件
- `pi-app-handle-mouse-events!` - 批量合并与处理鼠标事件队列

### 四、消息提交流程
- `pi-app-submit!` - 提交用户输入消息
- `pi-app-submit-initial!` - 提交初始 prompt
- `pi-app-drain-queue!` - 排空并处理待发消息队列
- `pi-app-add-user-message!` - 追加用户消息卡片
- `pi-app-add-assistant-text!` - 追加助手流式回复文本
- `pi-app-add-tool-result!` - 追加工具调用结果卡片
- `pi-app-add-retry-message!` - 追加重试提示卡片

### 五、通道与异步事件
- `pi-app-jobs-chan` - 获取任务分发通道
- `pi-app-event-chan` - 获取异步事件通道
- `pi-app-abort-chan` - 获取中断控制通道
- `pi-app-bash-jobs-chan` - 获取 Bash 任务通道
- `pi-app-poll-events!` - 轮询并派发异步通道事件

### 六、模式与思考状态
- `pi-app-bash-mode?` - 是否处于 Bash 命令直发模式
- `pi-app-thinking-level` - 获取当前模型思考级别
- `pi-app-supports-thinking?` - 当前模型是否支持深度思考
- `pi-app-cycle-thinking!` - 循环切换思考级别
- `pi-app-toggle-thinking!` - 切换思考内容折叠状态
- `pi-app-hide-thinking?` - 思考内容是否折叠隐藏
- `pi-app-default-thinking-level` - 获取当前默认思考级别
- `pi-app-available-thinking-levels` - 获取当前支持的思考级别列表

### 七、工具与命令
- `pi-app-tools-expanded?` - 工具详细输出是否展开
- `pi-app-toggle-tool-output!` - 切换工具输出展开状态
- `pi-app-run-bash!` - 调度执行单条 Bash 命令
- `pi-app-active-selector` - 获取当前激活的交互选择器组件
- `pi-app-slash-command-menu` - 获取斜杠命令菜单组件

### 八、模型与供应商
- `pi-app-provider` - 获取当前模型的供应商名称
- `pi-app-model` - 获取当前模型信息
- `pi-app-models` - 获取已激活模型列表
- `%pi-app-job-payload` - 构造任务调用载荷

### 九、会话恢复
- `pi-app-resume-command?` - 判定是否为恢复会话命令
- `pi-app-submit-resume!` - 提交恢复会话命令
- `pi-app-handle-resume-event!` - 处理会话恢复选择事件

### 十、视口与滚动
- `pi-app-scroll-view` - 获取对话记录滚动视口组件
- `pi-app-scroll-up!` - 视口向上滚动一页
- `pi-app-scroll-down!` - 视口向下滚动一页
- `pi-app-scroll-top!` - 视口滚动至顶部
- `pi-app-scroll-bottom!` - 视口滚动至底部
- `pi-app-scroll-line-up!` - 视口向上滚动一行
- `pi-app-scroll-line-down!` - 视口向下滚动一行
- `pi-app-frame-lines` - 计算当前帧渲染行集合
- `pi-app-transcript-lines` - 导出对话完整文本行列表
- `pi-app-wheel-accelerator` - 滚轮加速度计算器
