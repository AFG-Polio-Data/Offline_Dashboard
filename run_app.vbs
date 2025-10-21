Set shell = CreateObject("WScript.Shell")
currentPath = CreateObject("Scripting.FileSystemObject").GetParentFolderName(WScript.ScriptFullName)
shell.CurrentDirectory = currentPath
shell.Run Chr(34) & currentPath & "\run_app.bat" & Chr(34), 1, False

