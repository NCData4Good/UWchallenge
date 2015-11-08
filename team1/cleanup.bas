Sub GetGoogleData()
'Retrieve Google Geo data for agency program rows
    Static oXMLHTTP As Object
    Dim strHTML As String
    Dim oRE_LatLon As Object
    Dim oRE_Addrs As Object
    Dim oMatches As Object
    Dim oMatches_Addrs As Object
    Dim oM As Object
    Dim oSM As Object
    Dim oSM_Addrs As Object
    Dim lngSM As Long
    Dim sngstart As Single
    Dim strParsed() As String
'This URL requires a Google developer key
'    Const cURL As String = "https://maps.googleapis.com/maps/api/geocode/json?key=xxxxxxxxxxxxxxxxxxxxxxxxxxxxx&latlng=%LAT%,%LON%&sensor=true"

'This URL is free - mind your limits
    Const cURL As String = "https://maps.googleapis.com/maps/api/geocode/json?address="

    Const cPostalCode As String = """postal_code"""
    Const cStreetAddr As String = """street_address"""
    Const cRouteAddr As String = """route"""
    Const cCounty As String = "*""administrative_area_level_2""*"
    Const cZip As String = """postal_code"""
    Const cZipPlus4 As String = """postal_code_suffix"""
    Dim strURL As String
    Dim lngPage As Long
    Dim rng As Range
    Dim wks As Worksheet
    
    Set wks = Workbooks("agency_programs.xls").Worksheets("agency_programs")
    
    Set oRE_Addrs = CreateObject("vbscript.regexp")
    oRE_Addrs.Global = True
    oRE_Addrs.Pattern = "{\s*""long_name"" : ""([^""]*)"",\s*""short_name"" : ""([^""]*)"",\s*""types"" : \[ ([^\]]*) \]\s*}"
    Set oRE_LatLon = CreateObject("vbscript.regexp")
    oRE_LatLon.Global = True
    oRE_LatLon.Pattern = """formatted_address"" : ""(([^,]*)\s*,[^""]*)""(?:.|\n)*?""location"" : {\s*""lat"" : ([^,]*),\s*""lng"" : ([^\s]*)\s*}(?:.|\n)*?""types"" : \[ ([^\]]*) \]"

    
    If oXMLHTTP Is Nothing Then
        Set oXMLHTTP = CreateObject("MSXML2.XMLHTTP")
    End If

    Application.ScreenUpdating = False
    For Each rng In wks.Range(wks.Range("E2"), wks.Range("E2").End(xlDown)) 'iterate zips

        Do      '<<<GROUP

        If rng.Value = "NA" Then   'skip if zero lat value
            Exit Do
        End If
        If IsEmpty(rng.Offset(0, 5).Value) Then    'skip if already lat value
        Else
            Exit Do
        End If
        strURL = cURL & Join(Array(rng.Offset(0, -3).Value, rng.Offset(0, -2).Value, rng.Offset(0, -1).Value))
        strURL = Replace(WorksheetFunction.Trim(strURL), " ", "+")
        oXMLHTTP.Open "GET", strURL, False
        oXMLHTTP.Send
        
        Do Until oXMLHTTP.ReadyState = 4
            DoEvents
        Loop
        
        If oXMLHTTP.Status = 200 Then
            strHTML = oXMLHTTP.responsetext
            'parse the result
            If oRE_LatLon.test(strHTML) Then
                Set oMatches = oRE_LatLon.Execute(strHTML)
                For Each oM In oMatches
                    Select Case oM.submatches(4)
                        Case cStreetAddr, cRouteAddr, """subpremise""", """premise"""
                            rng.Offset(0, 5).Value = oM.submatches(2)
                            rng.Offset(0, 6).Value = oM.submatches(3)
                    End Select
                Next
            Else
                Debug.Print "LatLon Regex pattern does not match page for row: " & rng.Row
                Beep
            End If

            If oRE_Addrs.test(strHTML) Then
                Set oMatches = oRE_Addrs.Execute(strHTML)
                For Each oM In oMatches
                    If oM.submatches(2) Like cCounty Then
                        rng.Offset(0, 7).Value = oM.submatches(0)
                    End If
                    Select Case oM.submatches(2)
                        Case cZip
                            rng.Offset(0, 8).Value = oM.submatches(0)
                        Case cZipPlus4
                            rng.Offset(0, 9).Value = oM.submatches(0)
                            
                    End Select
                Next
            Else
                Debug.Print "Addrs Regex pattern does not match page for row: " & rng.Row
                Beep
            End If
        Else
            Beep
            Stop
        End If
        Sleep 100

        Loop While False    '<<<GROUP

    Next
    Application.ScreenUpdating = True

End Sub

Sub AddGeoDataToNCFeatures()
'This code assumes that the NC_Features download has been opened in
'a workbook and saved with name=NC_Features_20151001.xls

    Static oXMLHTTP As Object
    Dim strHTML As String
    Dim oRE As Object
    Dim oRE_Page As Object
    Dim oMatches As Object
    Dim oMatches_Pages As Object
    Dim oM As Object
    Dim oSM As Object
    Dim oSM_Pages As Object
    Dim lngSM As Long
    Dim sngstart As Single
    Dim strParsed() As String
    Const cURL As String = "https://maps.googleapis.com/maps/api/geocode/json?key=AIzaSyDmhtzceb9ee2Xr9-y7iVVujI1yjkKSqTA&latlng=%LAT%,%LON%&sensor=true"
    Const cPostalCode As String = """postal_code"""
    Const cStreetAddr As String = """street_address"""
    Const cRouteAddr As String = """route"""
    Dim strURL As String
    Dim lngPage As Long
    Dim rng As Range
    Dim wks As Worksheet
    
    Set wks = Workbooks("NC_Features_20151001.xls").Worksheets("NC_Features_20151001")
    
    Set oRE_Page = CreateObject("vbscript.regexp")
    oRE_Page.Global = True
    oRE_Page.Pattern = ">Showing (\d+) - (\d+) of (\d+)<"
    Set oRE = CreateObject("vbscript.regexp")
    oRE.Global = True
    oRE.Pattern = """formatted_address"" : ""(([^,]*)\s*,[^""]*)""(?:.|\n)*?""types"" : \[ ([^\]]*) \]"

    
    If oXMLHTTP Is Nothing Then
        Set oXMLHTTP = CreateObject("MSXML2.XMLHTTP")
    End If

    Application.ScreenUpdating = False
    For Each rng In wks.Range(wks.Range("J2"), wks.Range("J2").End(xlDown))
        Do      '<<<GROUP

        If rng.Value = 0 Then   'skip if zero lat value
            Exit Do
        End If
        If IsEmpty(rng.Offset(0, 12).Value) Then    'skip if already town value
        Else
            If IsEmpty(rng.Offset(0, 13).Value) Then
            Else
                Exit Do
            End If
        End If
        strURL = cURL
        strURL = Replace(strURL, "%LAT%", rng.Value)
        strURL = Replace(strURL, "%LON%", rng.Offset(0, 1).Value)
        oXMLHTTP.Open "GET", strURL, False
        oXMLHTTP.Send
        
        Do Until oXMLHTTP.ReadyState = 4
            DoEvents
        Loop
        
        If oXMLHTTP.Status = 200 Then
            strHTML = oXMLHTTP.responsetext
            'parse the result
            If oRE.test(strHTML) Then
                Set oMatches = oRE.Execute(strHTML)
                For Each oM In oMatches
                    Select Case oM.submatches(2)
                        Case cPostalCode
                            rng.Offset(0, 12).Value = oM.submatches(1)
                            If IsEmpty(rng.Offset(0, 13).Value) Then
                            Else
                                Exit For
                            End If
                        Case cStreetAddr, cRouteAddr
                            rng.Offset(0, 13).Value = oM.submatches(0)
                            If IsEmpty(rng.Offset(0, 12).Value) Then
                            Else
                                Exit For
                            End If
                    End Select
                Next
            Else
                Debug.Print "Regex pattern does not match page for row: " & rng.Row
                Beep
            End If

        End If
        Sleep 100

        Loop While False    '<<<GROUP

    Next
    Application.ScreenUpdating = True
End Sub
