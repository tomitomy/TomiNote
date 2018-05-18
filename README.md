
TomiNote v1.1

TomiNote is a simple note collection tool that supports the following features:

- Directory Tree
- Multiple nodes' history record
- Dynamic load child nodes (Load child nodes when expanding the node)
- Recycle bin
- Drag and drop node (Need to hold down Ctrl or Alt key)
- Move Left and Move Right the node
- Sort nodes
- Search and Replace in Multiple nodes (In a separate thread)
- Import and Export (File, Directories, Database)
- Auto-Save and Auto-Backup
- Split one node into multiple sub-nodes
- Script (Multiple replace, In a separate thread)
- Full Screen, Full Window, Toggle Control Bar
- Custom Font Name, Font Size, Foreground Color, Background Color
- International language support

For more features, please check the software help information or [wiki](https://github.com/tomitomy/TomiNote/wiki).

In this version, it is compatible with both Windows and Linux platforms. I did a simple test on "Ubuntu 16.04 64-bit" and "Windows 7 64-bit" and is currently work well.

Note: This program only supports characters in the range of "Unicode UCS-2" (0000 ~ FFFF). If there are characters beyond this range in your database, the function of "History" and "Search or Replace" will be abnormal.

　

The released Windows binary is compiled with Lazarus 1.8.0RC4.

The released GTK2 binary is compiled with Lazarus 1.8.0RC4 with the following patches applied:
https://bugs.freepascal.org/view.php?id=32583

If you are compiling this program from source, I suggest you use Lazarus 1.8.0RC4 and apply the above patch. because some Lazarus version can't process TMemo.SelLength:='' correctly, such as "Lazarus 1.8.0" and "Lazarus 1.8.2".

In short, if the following code can get correct result in your Lazarus, then your Lazarus should be able to compile out the correct TomiNote binary.

``` pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Text := 'abcd';
  Memo1.SelStart := 1;
  Memo1.SelLength := 1;
  Memo1.SelText := '';
  writeln(Memo1.Text);  // should be 'acd', if not, then your Lazarus can't compile out the correct TomiNote binary.
end;
```

You can download various versions of Lazarus here:
https://sourceforge.net/projects/lazarus/files/

　

There are 19 complete creation steps in the "src" directory:

- Step 1:
Create main form. (Just a shell without any actual function)

- Step 2:
Save and load configuration. (You can now save the window state)

- Step 3:
Option Dialog. (You can now easily modify the configuration)

- Step 4:
Add and Delete node. (Implement a basic Tree database)

- Step 5:
Recycle bin, Move node.

- Step 6:
Drag/Drop and Copy node.

- Step 7:
Dynamically load nodes, Save and Load note.

- Step 8:
Single node's history record.

- Step 9:
Multiple nodes' history record.

- Step 10:
Combine above functions together.

- Step 11:
Simple Search and Replace.

- Step 12:
Search and Replace in multiple nodes.

- Step 13:
Import and Export.

- Step 14:
Improve peripheral functions, such as: Load the last file at startup, recent files, auto save and auto backup, history size limit.

- Step 15:
Utils: Sort nodes, Split node, Rename nodes.

- Step 16:
Implement the script function.

- Step 17:
Search and Replace in a separate thread. (You can now abort the search process at any time)

- Step 18:
International language support

- Step 19:
Add help content. (This is the last step)

　

You can use a diff program to compare the differences between each steps. You can pick any step to develop another program if you want.

　

============================================================

　

TomiNote v1.1

TomiNote 是一个简单的笔记收集软件，它支持以下特性：

- 目录树
- 多节点历史记录
- 动态加载节点（展开节点的时候加载子节点）
- 回收站
- 拖放节点（需要按下 Ctrl 或 Alt 键）
- 左移和右移节点
- 节点排序
- 在多个节点中搜索和替换（使用独立的线程）
- 导入和导出（文件、目录、数据库）
- 自动保存和自动备份
- 将一个节点拆分成多个节点
- 脚本（多重替换，使用独立的线程）
- 全屏、满窗、切换工具栏
- 自定义字体名称、字体大小、前景色、背景色
- 国际化语言支持

更多特性请查看软件的帮助信息或[维基帮助](https://github.com/tomitomy/TomiNote/wiki).

在这个版本中，它兼容 Windows 和 Linux 两个平台，我在这两个平台上作了简单的测试，到目前为止工作良好。

注意：本程序仅支持“Unicode UCS-2”范围内的字符（0000～FFFF）。如果你的数据库中存在超出该范围的字符，则“历史记录”和“搜索替换”功能将出现异常。

　

Windows 版本的二进制文件是使用 Lazarus 1.8.0RC4 编译的。

GTK2 版本的二进制文件是使用 Lazarus 1.8.0RC4 编译的，并且应用了下面的补丁：
https://bugs.freepascal.org/view.php?id=32583

如果你想从源代码编译本程序，我建议你使用 Lazarus 1.8.0RC4 并应用上面提到的补丁。因为一些 Lazarus 版本不能正确处理 TMemo.SelLength:=''，比如 Lazarus 1.8.0 和 Lazarus 1.8.2。

总之，如果下面的代码在你的 Lazarus 中可以得到正确的结果，那么你的 Lazarus 应该就可以编译出正确的 TomiNote 二进制文件：

``` pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Text := 'abcd';
  Memo1.SelStart := 1;
  Memo1.SelLength := 1;
  Memo1.SelText := '';
  writeln(Memo1.Text);  // 应该得到“acd”，如果不是，那么你的 Lazarus 就不能编译出正确的 TomiNote 二进制文件。
end;
```

你可以在这里下载各个版本的 Lazarus：
https://sourceforge.net/projects/lazarus/files/

　

在 src 目录中保留了完整的 19 个创建步骤：

- 步骤 1:
创建主窗体（只是一个空壳，没有任何实际的功能）

- 步骤 2:
保存和在入配置（你现在可以保存窗口状态了）

- 步骤 3:
选项对话框（你现在可以很容易的修改配置了）

- 步骤 4:
添加和删除节点（实现了一个基本的树形数据库）

- 步骤 5:
回收站、移动节点

- 步骤 6:
拖放和复制节点

- 步骤 7:
动态载入节点、保存和载入笔记

- 步骤 8:
单个节点的历史记录

- 步骤 9:
多个节点的历史记录

- 步骤 10:
将上述功能组合到一起

- 步骤 11:
简单的搜索和替换

- 步骤 12:
在多个节点中搜索和替换

- 步骤 13:
导入和导出

- 步骤 14:
实现周边功能，例如：启动时载入最后的文件、最近打开的文件、自动保存和自动备份、历史记录大小限制

- 步骤 15:
工具：节点排序、拆分节点、重命名节点

- 步骤 16:
实现脚本功能

- 步骤 17:
在单独的线程中搜索和替换（你现在可以随时终止搜索过程）

- 步骤 18:
国际化语言支持

- 步骤 19:
添加帮助信息（这是最后一步）

　

您可以使用对比程序来比较每个步骤之间的差异。如果你愿意，你可以挑选任何一个步骤来开发另一个程序。

　

　

　

![01](https://github.com/tomitomy/TomiNote/blob/master/resources/images/01.png)

　

　

　

![02](https://github.com/tomitomy/TomiNote/blob/master/resources/images/02.png)

　

　

　

![03](https://github.com/tomitomy/TomiNote/blob/master/resources/images/03.png)

　

　

　

![04](https://github.com/tomitomy/TomiNote/blob/master/resources/images/04.png)

　

　

　

![05](https://github.com/tomitomy/TomiNote/blob/master/resources/images/05.png)

　

　

　

![05](https://github.com/tomitomy/TomiNote/blob/master/resources/images/06.png)
