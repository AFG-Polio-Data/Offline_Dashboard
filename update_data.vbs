Set shell = CreateObject("WScript.Shell")
shell.Run "cmd /c """ & Replace(WScript.ScriptFullName, "update_data.vbs", "update_data.bat") & """", 1, True
