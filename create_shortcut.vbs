Set objShell = WScript.CreateObject("WScript.Shell")
Set objShortcut = objShell.CreateShortcut(objShell.SpecialFolders("Desktop") & "\CBMS Table Generator.lnk")
objShortcut.TargetPath = "C:\cbms-table-generator\CBMS Table Generator.exe"
objShortcut.WorkingDirectory = "C:\cbms-table-generator"
objShortcut.Description = "CBMS Table Generator"
objShortcut.Save()
