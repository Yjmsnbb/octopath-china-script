' ------------------------------------------------------------------------------------------
' -------------------------------------- 全局变量与初始化 -----------------------------------
' ------------------------------------------------------------------------------------------

' --- 全局配置变量 (从UI读取) ---
Dim cfg_flower, cfg_flower_1, cfg_flower_2, cfg_flower_target
Dim cfg_nut, cfg_nut_target
Dim cfg_limit, cfg_limit_count
Dim cfg_harvest, cfg_harvest_mode
Dim cfg_shop, cfg_resolution

' --- 运行控制变量 ---
Dim scaleX, scaleY ' 缩放比例
Dim app_pkg
Dim total_start_time
Dim phase_start_time
Dim current_phase_name 
current_phase_name = "初始化"
Dim ambiguity 
ambiguity = 0.9 ' 颜色容差
Dim app ' 当前应用包名
Dim intX, intY ' 找图返回坐标

' --- 初始化 ---
Call InitScript()

' ------------------------------------------------------------------------------------------
' -------------------------------------- 主程序入口 -----------------------------------------
' ------------------------------------------------------------------------------------------

Sub Main()
    ' 1. 启动游戏并进入世界
    Call UpdateStatus("正在启动", "检查游戏状态")
    Call StartGameLogic()
    
    ' --- 主任务流程 (单次执行，无死循环) ---
    
    ' --- [步骤1] 漏刷检测预处理 (获取初始点数) ---
    Dim start_collection_num
    start_collection_num = 0
    Dim expected_drop
    expected_drop = 0
    Dim do_leak_check
    do_leak_check = False
    
    ' 只有在开启自动收获且选择了"检测漏刷模式"(索引0)时才执行
    If cfg_harvest And (CInt(cfg_harvest_mode) = 0) Then 
        do_leak_check = True
        Call CheckAndGoToTown() ' 确保在安全位置
        
        Call LogUI("漏刷检测", "正在读取初始点数...")
        start_collection_num = GetMapCollectionCount()
        
        ' 基础消耗计算：根据勾选的具体任务累加
        If cfg_flower Then
            If cfg_flower_1 Then expected_drop = expected_drop + 1
            If cfg_flower_2 Then expected_drop = expected_drop + 1
        End If
        
        If cfg_nut Then expected_drop = expected_drop + 1
        
        Call LogUI("漏刷检测", "初始:" & start_collection_num & " 预计消耗:" & expected_drop)
    End If

    ' --- [步骤2] 执行核心任务 ---
    If cfg_flower Then Call Task_FlowerField(cfg_flower_target)
    If cfg_nut Then Call Task_NutFarm(cfg_nut_target)
    
    ' --- [步骤3] 漏刷比对与补救 ---
    ' 无论是否检测漏刷，只要开启了自动收获，最后都需要尝试收集
    If cfg_harvest Then
        Dim need_remedy
        need_remedy = False ' 是否需要补救(重跑任务)
        
        If do_leak_check Then
            Call CheckAndGoToTown()
            
            Dim end_collection_num
            end_collection_num = GetMapCollectionCount()
            
            Dim actual_drop
            actual_drop = start_collection_num - end_collection_num
            
            Call LogUI("数据比对", "预计消耗" & expected_drop & " 实际" & actual_drop)
            
            ' 如果实际消耗 < 预计消耗，说明有任务没刷出来
            If actual_drop < expected_drop Then
                need_remedy = True
                Call LogUI("状态异常", "检测到漏刷，准备重跑")
            Else
                Call LogUI("状态正常", "计数匹配，准备收集")
            End If
        End If
        
        ' --- 补救逻辑：重新执行任务 ---
        If need_remedy Then
            Call LogUI("补救模式", "正在重跑任务...")
            ' 再次尝试执行勾选的任务
            If cfg_flower Then Call Task_FlowerField(cfg_flower_target)
            If cfg_nut Then Call Task_NutFarm(cfg_nut_target)
            Call LogUI("补救结束", "即将执行最终收集")
        End If
        
        ' --- [步骤4] 执行最终收集 ---
        Call Task_Collect()
    End If
    
    ' --- [步骤5] 额外任务：商店 ---
    If cfg_shop Then
        Call Task_MemoryShards()
    End If
    
    Call LogUI("脚本结束", "所有任务执行完毕")
    ' 脚本自然结束
End Sub

Call Main()

' ------------------------------------------------------------------------------------------
' -------------------------------------- 任务逻辑函数 ---------------------------------------
' ------------------------------------------------------------------------------------------

' 任务：花田
Function Task_FlowerField(targetNum)
    Call SetPhase("花田采集")
    
    Call CheckAndGoToTown()
    Call MoveMinimap(750, 630) ' 走到花田中间

    ' 根据配置执行花田1
    If cfg_flower_1 Then
        Call ProcessFlowerUnit(550, 800, "花田1", targetNum)
    End If
    
    ' 根据配置执行花田2
    If cfg_flower_2 Then
        Call ProcessFlowerUnit(550, 1090, "花田2", targetNum)
    End If
End Function

Sub ProcessFlowerUnit(clickX, clickY, name, target)
    Dim isDone
    isDone = False
    Dim retry
    retry = 0
    
    While isDone = False And retry < 3
        Call UpdateStatus(current_phase_name, "点击" & name)
        Call Click(clickX, clickY)
        
        If WaitImage("check-flowerarea", 5) Then ' 缩短等待时间提高响应
            Dim ocrStr
            Delay 500
            ' OCR识别数量
            ocrStr = GetTextFromScreen(550, 1060, 600, 1100) 
            
            Call UpdateStatus(current_phase_name, "识别数量: " & ocrStr)
            
            If IsNumeric(ocrStr) Then
                If CInt(ocrStr) <= CInt(target) Then
                    Call Click(430, 960) ' 确认收取
                    Call UpdateStatus(current_phase_name, name & "收取成功")
                    isDone = True
                Else
                     Call UpdateStatus(current_phase_name, "数量(" & ocrStr & ") > 目标")
                     isDone = True 
                End If
            Else
                Call UpdateStatus(current_phase_name, "识别非数字，重试")
                retry = retry + 1
            End If
        Else
             Call UpdateStatus(current_phase_name, "无弹窗，可能已收")
             isDone = True
        End If
    Wend
End Sub

' 任务：果炎
Function Task_NutFarm(targetNum)
    Call SetPhase("果炎采集")
    
    Call CheckAndGoToTown()
    Call OpenMap()
    Call TpTown(998) ' 传至圣树之泉
    
    Call MoveMinimap(722, 677)
    
    Dim isDone
    isDone = False
    Dim retry
    retry = 0
    
    While isDone = False And retry < 3
        Call UpdateStatus(current_phase_name, "点击果炎")
        Call Click(450, 562)
        
        If WaitImage("check-fruit", 5) Then
             If MatchPic("100") Then
                Call Click(430, 960)
                Call UpdateStatus(current_phase_name, "果炎收取完成")
                isDone = True
             Else
                Dim num
                num = GetTextFromScreen(550, 850, 600, 1140)
                If IsNumeric(num) And CInt(num) >= CInt(targetNum) Then
                     Call Click(430, 960)
                     isDone = True
                Else
                     Call UpdateStatus(current_phase_name, "未满(" & num & ")")
                     ' 果炎没满通常不需要重启，直接重跑即可，或者根据您的原逻辑重启
                     ' 这里为了单次运行效率，我们只重置位置重试，不强制重启游戏，除非完全卡死
                     Call CheckAndGoToTown()
                     Call OpenMap()
                     Call TpTown(998)
                     Call MoveMinimap(722, 677)
                     retry = retry + 1
                End If
             End If
        Else
            Call UpdateStatus(current_phase_name, "无反应")
            isDone = True
        End If
    Wend
End Function

' 获取地图上的采集计数
Function GetMapCollectionCount()
    Call SetPhase("数据读取")
    Call OpenMap()
    Delay 1000
    
    ' 1. 打开道具回收页面
    Call Click(120, 334)
    Delay 1500
    
    ' 2. 识别数字 (坐标: 780, 1180, 831, 1236)
    Dim num
    num = GetTextFromScreen(780, 1180, 831, 1236) 
    
    ' 3. 关闭道具回收页面
    Call Click(189, 724)
    Delay 500
    
    Call CloseMap()
    
    If IsNumeric(num) Then
        GetMapCollectionCount = CInt(num)
    Else
        Call LogUI("读取失败", "无法识别数字: " & num)
        GetMapCollectionCount = 0
    End If
End Function

' 任务：收集
Function Task_Collect()
    Call SetPhase("自动收获")
    If Not InMap() Then Call OpenMap()
    Delay 1000
    
    Call UpdateStatus(current_phase_name, "打开收集面板")
    Call Click(120, 334)
    Delay 1500
    
    Call UpdateStatus(current_phase_name, "执行收取")
    Call Click(194, 1193)
    Delay 2000
    
    Call Click(189, 724)
    Delay 1000
    
    Call CloseMap()
    Call UpdateStatus(current_phase_name, "收集操作完成")
End Function

' 任务：商店
Function Task_MemoryShards()
    Call SetPhase("商店购物")
    Call CheckAndGoToTown()
    
    Call LogUI("跑图", "前往旅店...")
    Call Click(860, 1680)
    Delay 500        
    Call Click(300, 1000)
    Delay 22000
    
    Call Click(860, 1680)
    Delay 500
    Call Click(650, 1100)
    Delay 8000 
    
    Call Click(500, 520)
    Delay 2000
    Call Click(120, 1800)
    Delay 2000
    Call Click(660, 1600)
    
    Dim i
    For i = 1 To 3
        Call Click(220, 1000)
        Delay 500
        Call Click(280, 1200)
        Delay 1000
    Next
    
    Call Click(430, 1000)
    Call LogUI("商店", "购买完成")
    
    For i = 1 To 3
        Call Click(1050, 1850)
        Delay 1000
    Next
End Function


' ------------------------------------------------------------------------------------------
' -------------------------------------- UI与初始化 -----------------------------------------
' ------------------------------------------------------------------------------------------

Sub InitScript()
    ' [关键] 提前初始化时间，防止LogUI报错
    total_start_time = TickCount()
    phase_start_time = TickCount()

    ' 1. 读取配置
    cfg_flower = ReadUIConfig("chk_flower")
    cfg_flower_1 = ReadUIConfig("chk_flower_1") ' 新增花田1
    cfg_flower_2 = ReadUIConfig("chk_flower_2") ' 新增花田2
    cfg_flower_target = ReadUIConfig("drop_flower_count")
    cfg_nut = ReadUIConfig("chk_nut")
    cfg_nut_target = ReadUIConfig("drop_nut_count")
    cfg_limit = ReadUIConfig("chk_loop_limit")
    cfg_limit_count = ReadUIConfig("input_loop_count")
    cfg_harvest = ReadUIConfig("chk_harvest")
    cfg_harvest_mode = ReadUIConfig("drop_harvest_mode")
    cfg_shop = ReadUIConfig("chk_shop")
    cfg_resolution = ReadUIConfig("drop_resolution")
    
    ' 2. 计算缩放
    Call CalcScale(cfg_resolution)
    
    ' 3. 创建浮窗
    ' NewFWindow(WindowName, X, Y, W, H)
    FW.NewFWindow "浮窗", 0, CY(1050), CX(1920), CY(30)
    
    ' 添加控件: AddTextView(WindowName, ControlName, Text, X, Y, W, H)
    ' 左侧(Stats): 0 - 1400
    FW.AddTextView "浮窗", "Stats", "初始化...", 0, 0, CX(1400), CY(30)
    
    ' 右侧(Action): 1400 - 1920 (宽度520)
    FW.AddTextView "浮窗", "Action", "...", CX(1400), 0, CX(520), CY(30)
    
    ' 设置属性
    FW.Opacity "浮窗", 0
    FW.SetTextColor "Stats", "FFFFFF"
    FW.SetTextSize "Stats", 15
    FW.SetTextColor "Action", "FFFFFF"
    FW.SetTextSize "Action", 15
    
    ' 最后显示窗口
    FW.Show "浮窗"
End Sub

Sub CalcScale(index)
    Dim w, h
    Select Case CInt(index)
        Case 0: w=960 : h=540
        Case 1: w=1280 : h=720
        Case 2: w=1920 : h=1080
        Case 3: w=2560 : h=1440
        Case 4: w=3840 : h=2160
        Case Else: w=1920 : h=1080
    End Select
    
    scaleX = w / 1920
    scaleY = h / 1080
End Sub

Sub SetPhase(name)
    current_phase_name = name
    phase_start_time = TickCount()
    Call UpdateStatus(name, "开始...")
End Sub

Sub LogUI(phase, action)
    Dim total_sec, phase_sec
    total_sec = Int((TickCount() - total_start_time) / 1000)
    phase_sec = Int((TickCount() - phase_start_time) / 1000)
    
    ' 格式化左侧信息: [模式] 本段耗时 | 总耗时
    Dim statsText
    statsText = "[" & phase & "] " & phase_sec & "s (总:" & Int(total_sec/60) & "m)"
    
    ' 打印调试信息
    TracePrint statsText & " | " & action
    
    ' [关键修复] SetTextView 必须带上坐标参数才能在您的环境中生效
    ' 语法: SetTextView(ControlName, Text, X, Y, W, H)
    ' 左侧 Stats 更新
    FW.SetTextView "Stats", statsText, 0, 0, CX(1400), CY(30)
    
    ' 右侧 Action 更新
    FW.SetTextView "Action", action, CX(1400), 0, CX(520), CY(30)
    
    Delay 50 
End Sub

Sub UpdateStatus(phase, action)
    Call LogUI(phase, action)
End Sub

' ------------------------------------------------------------------------------------------
' -------------------------------------- 基础操作封装 ---------------------------------------
' ------------------------------------------------------------------------------------------

Function CX(x)
    CX = Int(x * scaleX)
End Function

Function CY(y)
    CY = Int(y * scaleY)
End Function

Function GetTextFromScreen(x1, y1, x2, y2)
    Dim txt
    txt = Image.OcrText(CX(x1), CY(y1), CX(x2), CY(y2), 0, 1)
    GetTextFromScreen = Replace(txt, " ", "")
End Function

Function Click(x, y)
    Touch CX(x), CY(y), 100
    Delay 400
End Function

Function SlowClick(x, y)
    Touch CX(x), CY(y), 200
    Delay 800
End Function

Function Roll(x1, y1, x2, y2)
    TouchDown CX(x1), CY(y1)
    Delay 100
    TouchMove CX(x2), CY(y2), 0, 200
    Delay 100
    TouchUp 
    Delay 400
End Function

Function MatchPic(picName)
    FindPic 0, 0, CX(1080), CY(1920), "Attachment:" & picName & ".png", "000000", 0, 0.8, intX, intY
    If intX > -1 Then 
        MatchPic = True 
    Else 
        MatchPic = False
    End If
End Function

Function MatchPointColor(x, y, color, range)
    Dim fX, fY
    FindColor CX(x) - range, CY(y) - range, CX(x) + range, CY(y) + range, color, 0, ambiguity, fX, fY
    If fX > -1 Then 
        MatchPointColor = True 
    Else 
        MatchPointColor = False
    End If
End Function

Function WaitImage(picName, timeout)
    Dim t
    t = 0
    While t < timeout * 5
        If MatchPic(picName) Then
            WaitImage = True
            Exit Function
        End If
        Delay 200
        t = t + 1
    Wend
    WaitImage = False
End Function

' ------------------------------------------------------------------------------------------
' -------------------------------------- 游戏控制逻辑 ---------------------------------------
' ------------------------------------------------------------------------------------------

Function StartGameLogic()
    Dim pkg
    pkg = Sys.GetFront()
    If pkg = "com.netease.ma167" Then
        app = "com.netease.ma167"
        Call LogUI("启动", "网易服运行中")
    ElseIf pkg = "com.netease.ma167.bilibili" Then
        app = "com.netease.ma167.bilibili"
        Call LogUI("启动", "B服运行中")
    Else
        Call LogUI("启动", "启动游戏...")
        RunApp "com.netease.ma167"
        RunApp "com.netease.ma167.bilibili"
        Dim waitTime
        waitTime = 0
        While Not InWorld() And waitTime < 100
            Call Click(350, 700)
            Delay 500
            waitTime = waitTime + 1
        Wend
        app = Sys.GetFront()
        If waitTime >= 100 Then Call RestartGameLogic()
    End If
End Function

Function RestartGameLogic()
    Call LogUI("重启", "重启游戏...")
    KillApp app
    Delay 2000
    RunApp app
    Dim waitTime
    waitTime = 0
    While Not InWorld() And waitTime < 100
        Call Click(1050, 30)
        Delay 500
        waitTime = waitTime + 1
    Wend
End Function

Function BackToWorld()
    Dim retry
    retry = 0
    While True
        If Sys.GetFront() <> app Then
            Call RestartGameLogic()
        ElseIf InMap() Then
            Call CloseMap()
        ElseIf InWorld() Then
            Exit Function
        Else
            Call Click(1057, 1884)
            Delay 1000
        End If
        retry = retry + 1
        If retry > 15 Then
            Call RestartGameLogic()
            retry = 0
        End If
        Delay 1000
    Wend
End Function

Function CheckAndGoToTown()
    Call BackToWorld()
    Call Click(874, 1618)
    Delay 500
    Dim mapName
    mapName = GetTextFromScreen(950, 150, 1010, 350)
    If mapName <> "无名小镇" And Not MatchPic("Door") Then
        Call LogUI("传送", "前往无名小镇")
        Call BackToWorld()
        Call OpenMap()
        Call TpTown(0)
    Else
        Call BackToWorld()
    End If
End Function

Function TpTown(n)
    If InMap() Then
        Call MapStandardization()
        Select Case n
        Case 0 
            Call Click(823, 1157)
            Call LogUI("传送", "无名小镇")
        Case 998 
            Call SlowClick(823, 1157) 
            Call SlowClick(1050, 62)  
            Call SlowClick(107, 1812) 
            Call SlowClick(710, 1115) 
            Call LogUI("传送", "圣树之泉")
        Case Else
            Call CloseMap()
            Exit Function
        End Select
        Delay 1000
        Call SlowClick(126, 1615)
        If n = 998 Then Call SlowClick(700, 960) 
        Call Click(350, 1198) 
        Dim waitMap
        waitMap = 0
        While InGame() And Not InWorld() And waitMap < 30
            Delay 1000
            waitMap = waitMap + 1
        Wend
    End If
End Function

Function InWorld()
    If MatchPointColor(218, 96, "F6F6F5", 1) And MatchPointColor(150, 73, "0D0D0D", 1) Then 
        InWorld = True 
    Else 
        InWorld = False
    End If
End Function

Function InMap()
    If MatchPointColor(1041, 832, "4779A9", 1) And MatchPointColor(1037, 1144, "86BFE3", 1) Then 
        InMap = True 
    Else 
        InMap = False
    End If
End Function

Function InGame()
    If app = Sys.GetFront() Then 
        InGame = True 
    Else 
        InGame = False
    End If
End Function

Function OpenMap()
    If InWorld() Then
        Dim wait
        wait = 0
        While InGame() And Not InMap() And wait < 10
            If MatchPointColor(174, 1835, "A24645", 1) Or MatchPointColor(174, 1835, "427D91", 1) Then 
                Call Click(150, 1125) 
            Else 
                Call Click(150, 1290)
            End If
            Delay 1000
            wait = wait + 1
        Wend
    End If
End Function

Function CloseMap()
    If InMap() Then
        Call Click(1048, 1854)
        Dim wait
        wait = 0
        While InGame() And Not InWorld() And wait < 10
            Delay 1000
            Call Click(1048, 1854)
            wait = wait + 1
        Wend
    End If
End Function

Function MapStandardization()
    If InMap() Then
        If MatchPic("secondContinent") Then 
            Call SlowClick(913, 1812)
            Call SlowClick(354, 1193)
            Delay 2000
            If InWorld() Then Call OpenMap()
        End If
        If MatchPic("sideHell") Then Call SlowClick(836, 113)
        If Not MatchPointColor(118, 1815, "000000", 1) Then Call Click(108, 1815)
        Call Roll(920, 171, 171, 1671)
        Call Roll(920, 171, 171, 1671)
        Call Roll(119, 1711, 861, 891)
    End If
End Function

Function MoveMinimap(x, y)
    Dim loopTime
    loopTime = 0
    While InWorld() = False And loopTime < 30
        Call Click(1048, 1855)
        Delay 500
        loopTime = loopTime + 1
    Wend
    loopTime = 0
    While InGame() And Not WhereMinimap(x, y) And loopTime < 60
        Delay 1000
        loopTime = loopTime + 1
    Wend
End Function

Function WhereMinimap(x, y)
    If InWorld() Then
        Call Click(874, 1618) 
        Delay 500 
        If MatchPointColor(x, y, "7ED5E2", 5) Then
            WhereMinimap = True
            Call Click(1048, 1854) 
        Else
            Call SlowClick(x, y) 
        End If
    End If
End Function
