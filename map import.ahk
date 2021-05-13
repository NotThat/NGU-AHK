#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
CoordMode, Mouse, Client ;for higher than 1080p resolutions

SleepTime := 50
version := "0.3" ;;window size agnostic
Author := "Meowchan"


; JSON for AutoHotkey
; Copyright (c) 2018 Kurt McKee <contactme@kurtmckee.org>
; The code is licensed under the terms of the MIT license.

; VERSION = "1.0"

Jxon_Load(ByRef src, args*)
{
	static q := Chr(34)

	key := "", is_key := false
	stack := [ tree := [] ]
	is_arr := { (tree): 1 }
	next := q . "{[01234567890-tfn"
	pos := 0
	while ( (ch := SubStr(src, ++pos, 1)) != "" )
	{
		if InStr(" `t`n`r", ch)
			continue
		if !InStr(next, ch, true)
		{
			ln := ObjLength(StrSplit(SubStr(src, 1, pos), "`n"))
			col := pos - InStr(src, "`n",, -(StrLen(src)-pos+1))

			msg := Format("{}: line {} col {} (char {})"
			,   (next == "")      ? ["Extra data", ch := SubStr(src, pos)][1]
			  : (next == "'")     ? "Unterminated string starting at"
			  : (next == "\")     ? "Invalid \escape"
			  : (next == ":")     ? "Expecting ':' delimiter"
			  : (next == q)       ? "Expecting object key enclosed in double quotes"
			  : (next == q . "}") ? "Expecting object key enclosed in double quotes or object closing '}'"
			  : (next == ",}")    ? "Expecting ',' delimiter or object closing '}'"
			  : (next == ",]")    ? "Expecting ',' delimiter or array closing ']'"
			  : [ "Expecting JSON value(string, number, [true, false, null], object or array)"
			    , ch := SubStr(src, pos, (SubStr(src, pos)~="[\]\},\s]|$")-1) ][1]
			, ln, col, pos)

			throw Exception(msg, -1, ch)
		}

		is_array := is_arr[obj := stack[1]]

		if i := InStr("{[", ch)
		{
			val := (proto := args[i]) ? new proto : {}
			is_array? ObjPush(obj, val) : obj[key] := val
			ObjInsertAt(stack, 1, val)
			
			is_arr[val] := !(is_key := ch == "{")
			next := q . (is_key ? "}" : "{[]0123456789-tfn")
		}

		else if InStr("}]", ch)
		{
			ObjRemoveAt(stack, 1)
			next := stack[1]==tree ? "" : is_arr[stack[1]] ? ",]" : ",}"
		}

		else if InStr(",:", ch)
		{
			is_key := (!is_array && ch == ",")
			next := is_key ? q : q . "{[0123456789-tfn"
		}

		else ; string | number | true | false | null
		{
			if (ch == q) ; string
			{
				i := pos
				while i := InStr(src, q,, i+1)
				{
					val := StrReplace(SubStr(src, pos+1, i-pos-1), "\\", "\u005C")
					static end := A_AhkVersion<"2" ? 0 : -1
					if (SubStr(val, end) != "\")
						break
				}
				if !i ? (pos--, next := "'") : 0
					continue

				pos := i ; update pos

				  val := StrReplace(val,    "\/",  "/")
				, val := StrReplace(val, "\" . q,    q)
				, val := StrReplace(val,    "\b", "`b")
				, val := StrReplace(val,    "\f", "`f")
				, val := StrReplace(val,    "\n", "`n")
				, val := StrReplace(val,    "\r", "`r")
				, val := StrReplace(val,    "\t", "`t")

				i := 0
				while i := InStr(val, "\",, i+1)
				{
					if (SubStr(val, i+1, 1) != "u") ? (pos -= StrLen(SubStr(val, i)), next := "\") : 0
						continue 2

					; \uXXXX - JSON unicode escape sequence
					xxxx := Abs("0x" . SubStr(val, i+2, 4))
					if (A_IsUnicode || xxxx < 0x100)
						val := SubStr(val, 1, i-1) . Chr(xxxx) . SubStr(val, i+6)
				}

				if is_key
				{
					key := val, next := ":"
					continue
				}
			}

			else ; number | true | false | null
			{
				val := SubStr(src, pos, i := RegExMatch(src, "[\]\},\s]|$",, pos)-pos)
			
			; For numerical values, numerify integers and keep floats as is.
			; I'm not yet sure if I should numerify floats in v2.0-a ...
				static number := "number", integer := "integer"
				if val is %number%
				{
					if val is %integer%
						val += 0
				}
			; in v1.1, true,false,A_PtrSize,A_IsUnicode,A_Index,A_EventInfo,
			; SOMETIMES return strings due to certain optimizations. Since it
			; is just 'SOMETIMES', numerify to be consistent w/ v2.0-a
				else if (val == "true" || val == "false")
					val := %value% + 0
			; AHK_H has built-in null, can't do 'val := %value%' where value == "null"
			; as it would raise an exception in AHK_H(overriding built-in var)
				else if (val == "null")
					val := ""
			; any other values are invalid, continue to trigger error
				else if (pos--, next := "#")
					continue
				
				pos += i-1
			}
			
			is_array? ObjPush(obj, val) : obj[key] := val
			next := obj==tree ? "" : is_array ? ",]" : ",}"
		}
	}

	return tree[1]
}

Jxon_Dump(obj, indent:="", lvl:=1)
{
	static q := Chr(34)

	if IsObject(obj)
	{
		static Type := Func("Type")
		if Type ? (Type.Call(obj) != "Object") : (ObjGetCapacity(obj) == "")
			throw Exception("Object type not supported.", -1, Format("<Object at 0x{:p}>", &obj))

		is_array := 0
		for k in obj
			is_array := k == A_Index
		until !is_array

		static integer := "integer"
		if indent is %integer%
		{
			if (indent < 0)
				throw Exception("Indent parameter must be a postive integer.", -1, indent)
			spaces := indent, indent := ""
			Loop % spaces
				indent .= " "
		}
		indt := ""
		Loop, % indent ? lvl : 0
			indt .= indent

		lvl += 1, out := "" ; Make #Warn happy
		for k, v in obj
		{
			if IsObject(k) || (k == "")
				throw Exception("Invalid object key.", -1, k ? Format("<Object at 0x{:p}>", &obj) : "<blank>")
			
			if !is_array
				out .= ( ObjGetCapacity([k], 1) ? Jxon_Dump(k) : q . k . q ) ;// key
				    .  ( indent ? ": " : ":" ) ; token + padding
			out .= Jxon_Dump(v, indent, lvl) ; value
			    .  ( indent ? ",`n" . indt : "," ) ; token + indent
		}

		if (out != "")
		{
			out := Trim(out, ",`n" . indent)
			if (indent != "")
				out := "`n" . indt . out . "`n" . SubStr(indt, StrLen(indent)+1)
		}
		
		return is_array ? "[" . out . "]" : "{" . out . "}"
	}

	; Number
	else if (ObjGetCapacity([obj], 1) == "")
		return obj

	; String (null -> not supported by AHK)
	if (obj != "")
	{
		  obj := StrReplace(obj,  "\",    "\\")
		, obj := StrReplace(obj,  "/",    "\/")
		, obj := StrReplace(obj,    q, "\" . q)
		, obj := StrReplace(obj, "`b",    "\b")
		, obj := StrReplace(obj, "`f",    "\f")
		, obj := StrReplace(obj, "`n",    "\n")
		, obj := StrReplace(obj, "`r",    "\r")
		, obj := StrReplace(obj, "`t",    "\t")

		static needle := (A_AhkVersion<"2" ? "O)" : "") . "[^\x20-\x7e]"
		while RegExMatch(obj, needle, m)
			obj := StrReplace(obj, m[0], Format("\u{:04X}", Ord(m[0])))
	}
	
	return q . obj . q
}


json_escape(blob)
{
    hexadecimal := "0123456789abcdef"

    escapes := {}
    escapes["`b"] := "\b"
    escapes["`f"] := "\f"
    escapes["`n"] := "\n"
    escapes["`r"] := "\r"
    escapes["`t"] := "\t"
    escapes["/"] := "\/"
    escapes["\"] := "\\"
    escapes[""""] := "\"""


    loop, % strlen(blob)
    {
        character := substr(blob, a_index, 1)
        value := ord(character)

        ; Use simple escapes for reserved characters.
        if (instr("`b`f`n`r`t/\""", character))
        {
            escaped_blob .= escapes[character]
        }

        ; Allow ASCII characters through without modification.
        else if (value >= 32 and value <= 126)
        {
            escaped_blob .= character
        }

        ; Use Unicode escapes for everything else.
        else
        {
            hex1 := substr(hexadecimal, ((value & 0xF000) >> 12) + 1, 1)
            hex2 := substr(hexadecimal, ((value & 0xF00) >> 8) + 1, 1)
            hex3 := substr(hexadecimal, ((value & 0xF0) >> 4) + 1, 1)
            hex4 := substr(hexadecimal, ((value & 0xF) >> 0) + 1, 1)
            escaped_blob .= "\u" . hex1 . hex2 . hex3 . hex4
        }
    }

    return escaped_blob
}


json_unescape(blob)
{
    escapes := {}
    escapes["b"] := "`b"
    escapes["f"] := "`f"
    escapes["n"] := "`n"
    escapes["r"] := "`r"
    escapes["t"] := "`t"
    escapes["/"] := "/"
    escapes["\"] := "\"
    escapes[""""] := """"


    index := 1
    loop
    {
        if (index > strlen(blob))
        {
            break
        }

        character := substr(blob, index, 1)
        next_character := substr(blob, index + 1, 1)
        if (character != "\")
        {
            unescaped_blob .= character
        }
        else if (instr("bfnrt/\""", next_character))
        {
            unescaped_blob .= escapes[next_character]
            index += 1
        }
        else if (next_character == "u")
        {
            unicode_character := chr("0x" . substr(blob, index + 2, 4))
            unescaped_blob .= unicode_character
            index += 5
        }

        index += 1
    }

    return unescaped_blob
}


json_get_object_type(object)
{
    ; Identify the object type and return either "dict" or "list".
    object_type := "list"
    if (object.length() == 0)
    {
        object_type := "dict"
    }
    for key in object
    {
        ; The current AutoHotkey list implementation will loop through its
        ; indexes in order from least to greatest. If the object can be
        ; represented as a list, each key will match the a_index variable.
        ; However, if it is a sparse list (that is, if it has non-consective
        ; list indexes) then it must be represented as a dict.
        if (key != a_index)
        {
            object_type := "dict"
        }
    }

    return object_type
}


json_dump(info)
{
    ; Differentiate between a list and a dictionary.
    object_type := json_get_object_type(info)

    for key, value in info
    {
        ; Only include a key if this is a dictionary.
        if (object_type == "dict")
        {
            escaped_key := json_escape(key)
            blob .= """" . escaped_key . """: "
        }

        if (isobject(value))
        {
            blob .= json_dump(value) . ", "
        }
        else
        {
            escaped_value := json_escape(value)
            blob .= """" . escaped_value . """, "
        }
    }

    ; Remove the final trailing comma.
    if (substr(blob, -1, 2) == ", ")
    {
        blob := substr(blob, 1, -2)
    }

    ; Wrap the string in brackets or braces, as appropriate.
    if (object_type == "list")
    {
        blob := "[" . blob . "]"
    }
    else
    {
        blob := "{" . blob . "}"
    }

    return blob
}


json_load(blob)
{
    blob_length := strlen(blob)
    index_left := 0
    index_right := 0

    ; Identify the object type.
    loop, % blob_length
    {
        index_left += 1

        if (substr(blob, a_index, 1) == "[")
        {
            object_type := "list"
            info := []
            break
        }
        else if (substr(blob, a_index, 1) == "{")
        {
            object_type := "dict"
            info := {}
            break
        }
    }

    ; Extract all key/value pairs.
    loop, % blob_length
    {
        ; Extract the key.
        ; Use an integer key if this is a list object.
        if (object_type == "list")
        {
            key := info.length() + 1
        }
        else
        {
            ; Find the left side of the key.
            loop, % blob_length
            {
                index_left += 1

                if (substr(blob, index_left, 1) == """")
                {
                    break
                }
            }

            index_right := index_left

            ; Find the right side of the key.
            loop, % blob_length
            {
                index_right += 1

                ; Skip escaped characters, in case they are quotation marks.
                if (substr(blob, index_right, 1) == "\")
                {
                    index_right += 1
                }
                else if (substr(blob, index_right, 1) == """")
                {
                    break
                }
            }

            ; Store the key.
            escaped_key := substr(blob, index_left + 1, index_right - index_left - 1)
            key := json_unescape(escaped_key)
        }

        ; Pass over whitespace and any colons that separate key-value pairs.
        index_left := index_right + 1
        loop, % blob_length
        {
            index_left += 1

            if (not instr("`b`f`n`r`t :", substr(blob, index_left, 1)))
            {
                break
            }
        }

        ; If the value isn't a string, adjust the left index to include
        ; the beginning of the literal, dictionary, or list.
        depth := 0
        in_string := true
        value_type := "str"
        index_right := index_left + 1
        if (substr(blob, index_left, 1) != """")
        {
            in_string := false
            value_type := "literal"
            if (substr(blob, index_left, 1) == "{")
            {
                depth := 1
                value_type := "dict"
            }
            else if (substr(blob, index_left, 1) == "[")
            {
                depth := 1
                value_type := "list"
            }

            index_left -= 1
        }

        ; Find the right edge of the value.
        ;
        ; The loop will isolate the entire value, whether it's a string,
        ; list, dictionary, boolean, integer, float, or null. For example:
        ;
        ;   *   "abc"
        ;   *   123
        ;   *   true
        ;   *   false
        ;   *   null
        ;   *   [123, {"abc": true}]
        ;   *   {"a": [123, null]}
        loop
        {
            if (index_right > blob_length)
            {
                return info
            }

            if (in_string)
            {
                ; If the right index is passing through a string and the
                ; closing quotation mark is encountered, flag that the index
                ; is no longer in a string, and exit the loop if the value is
                ; a string.
                if (substr(blob, index_right, 1) == """")
                {
                    in_string := false
                    if (value_type == "str")
                    {
                        break
                    }
                }
                ; If the right index encounters a backslash in a string, the
                ; next character is guaranteed to still be in the string. Move
                ; the right index forward an additional character in case
                ; the escaped character is a quotation mark.
                else if (substr(blob, index_right, 1) == "\")
                {
                    index_right += 1
                }
            }

            ; If the right index encounters a quotation mark but is not already
            ; in a string, flag that the index is now passing through a string.
            else if (substr(blob, index_right, 1) == """")
            {
                in_string := true
            }

            ; If the value is a dictionary, keep track of the depth of any
            ; nested dictionaries. If the final closing curly brace is found,
            ; move the right index forward so that the right curly brace will
            ; be included in the value and exit the loop.
            else if (value_type == "dict")
            {
                ; If the value is a dictionary
                if (substr(blob, index_right, 1) == "{")
                {
                    depth += 1
                }
                else if (substr(blob, index_right, 1) == "}")
                {
                    depth -= 1
                    index_right += 1
                    if (depth == 0)
                    {
                        break
                    }
                }
            }

            ; If the value is a list, keep track of the depth of any nested
            ; lists. If the final closing bracket is found, move the right
            ; index forward so that the right bracket will be included in
            ; the value and exit the loop.
            else if (value_type == "list")
            {
                if (substr(blob, index_right, 1) == "[")
                {
                    depth += 1
                }
                else if (substr(blob, index_right, 1) == "]")
                {
                    index_right += 1
                    depth -= 1
                    if (depth == 0)
                    {
                        break
                    }
                }
            }

            ; If the value is a literal, such as a boolean or integer, just
            ; watch for any character that will indicate that the end of the
            ; literal has been encountered.
            else if (value_type == "literal")
            {
                if (instr("`b`f`n`r`t ,]}", substr(blob, index_right, 1)))
                {
                    break
                }
            }

            index_right += 1
        }

        ; Extract the value, now that its left and right sides have been found.
        value := substr(blob, index_left + 1, index_right - index_left - 1)

        ; Recursively parse dictionaries and lists.
        if (value_type == "dict" or value_type == "list")
        {
            value := json_load(value)
        }
        ; Escape string values.
        else if (value_type == "str")
        {
            value := json_unescape(value)
        }
        ; Convert boolean and null literals to booleans.
        else if (value == "true")
        {
            value := true
        }
        else if (value == "false")
        {
            value := false
        }
        else if (value == "null")
        {
            value := false
        }

        ; Save the key/value pair.
        info[key] := value

        ; Move the index.
        index_left := index_right + 1
    }

    return info
}


setDirection(direction){
	Switch direction
	{
	Case "w":
		Send w
		return
	Case "a":
		Send a
		return
	Case "s":
		Send s
		return
	Case "d":
		Send d
		return
	}
}

Concatente2(x, y) {
    Return, x y
}

Concatente3(x, y, z) {
    Return, x y z
}

desiredShape(i){
	Switch i
	{
	Case 1:
		return "box"
	Case 2:
		return "knight"
	Case 3:
		return "arrow"
	Case 4:
		return "wall"
	Case 5:
		return "donut"
	Default:
		return ""
	}
}

desiredType(i){
	Switch i
	{
	Case 1:
		return "speed"
	Case 2:
		return "production"
	Case 3:
		return "cost"
	Default:
		return ""
	}
}

desiredDirection(i){
	Switch i
	{
	Case 1:
		return "w"
	Case 2:
		return "a"
	Case 3:
		return "s"
	Case 4:
		return "d"
	Default:
		return ""
	}
}

safeClick(Xcoord, Ycoord){
	rect := WindowGetRect("NGU INDUSTRIES")
	;MsgBox % rect.width "`n" rect.height
	
	if(Xcoord >= rect.width or Ycoord >= rect.height or Xcoord <= 0 or Ycoord <= 0){ ;dont press outside the window
		msgBox Error: Coordinates %Xcoord%, %Ycoord% outside of window
		return
	}
	MouseClick Left, Xcoord, Ycoord
	return
}

moveBeacon(type, shape, windowScaleX, windowScaleY){
	;150 pixels by 70 pixels buttons
	
	X := 290
	Y := 335
	XShift := 0
	YShift := 0
	XShiftButtonSize := 150
	YShiftButtonSize := 70
	
	Switch type{
		Case "speed":
			XShift := 0
		Case "production":
			XShift := 1
		Case "cost":
			XShift := 2
	}
	
	Switch shape{
		Case "box":
			YShift := 0
		Case "knight":
			YShift := 1
		Case "arrow":
			YShift := 2
		Case "wall":
			YShift := 3
		Case "donut":
			YShift := 4
	}
	
	safeClick((X+XShift*XShiftButtonSize)*windowScaleX, (Y+YShift*YShiftButtonSize)*windowScaleY)
	
	return
}

;!p::Pause  ;alt+p to pause/unpause

F1::
FileRead, blob, 1.json
data := Jxon_Load(blob)
Goto, buildMap

F2::
data := Jxon_Load(Clipboard)
Goto, buildMap

WindowGetRect(windowTitle*) {
	if hwnd := WinExist(windowTitle*) {
		VarSetCapacity(rect, 16, 0)
		DllCall("GetClientRect", "Ptr", hwnd, "Ptr", &rect)
		return {width: NumGet(rect, 8, "Int"), height: NumGet(rect, 12, "Int")}
	}
}

getWindowParameters(ByRef windowScaleX, ByRef windowScaleY){
	rect := WindowGetRect("NGU INDUSTRIES")
	;MsgBox % rect.width "`n" rect.height

	windowScaleX := rect.width / 1920
	windowScaleY := rect.height / 1080
	
	return
}

buildMap:

getWindowParameters(windowScaleX, windowScaleY) ;figure out window scales
safeClick(328*windowScaleX, 104*windowScaleY) ;beacons button
Sleep SleepTime*2

shapeIteration:=0
while(shapeIteration<=5){
	shapeIteration++
	shapeGoal := desiredShape(shapeIteration)
	
	typeIteration:=0
	while(typeIteration<=3){
		typeIteration++
		typeGoal := desiredType(typeIteration)
		
		directionIteration:=0
		while(directionIteration<=4){
			directionIteration++
			;if(beaconGoal = "knight" and directionIteration > 1) ;force knights upwards only
			;	continue
			directionGoal := desiredDirection(directionIteration)

			squareGoal := Concatente3(typeGoal, shapeGoal, directionGoal)			

			row:=0
			while(row<17){ ;rows go from 0 to 16
				column:=0
				while(column<20) ;columns go from 1 to 20
				{
					column++
					i:=(row)*20+column
					a := data.tiles[i].type
					
					if (a = ""){ ;null square (unusable), move on
						continue
					}
					
					if (a = "lab"){ ;'cog' square, no beacon, continue
						continue
					}
					
					;if (a != "beacon"){
					;	continue
					;}
					
					currentSquare := Concatente3(data.tiles[i].beaconType, data.tiles[i].shape, data.tiles[i].direction)
					
					if (squareGoal != currentSquare)
						continue
					
					;square is a beacon
					;beacon type
					beacontype := Concatente2(data.tiles[i].beaconType, data.tiles[i].shape)
					newBeacon := beacontype
					if (oldBeacon <> newBeacon){ ;only do this if beacon type changed to save time
						oldBeacon := newBeacon
						moveBeacon(data.tiles[i].beaconType, data.tiles[i].shape, windowScaleX, windowScaleY) ;select the desired beacon
						Sleep SleepTime
					}

					;direction
					newDirection := Send data.tiles[i].direction
					if (data.tiles[i].beaconType = "knight")
						newDirection = "w" ;force all knights to face upward
					if (oldDirection <> newDirection){ ;only do this if need to send new keypress to save time
						oldDirection := newDirection
						setDirection(newDirection)
						Sleep SleepTime
					}
					
					;60, 60 grid square size
					newX := (660 + column*60)*windowScaleX
					newY := (60 + row*60)*windowScaleY
					
					safeClick(newX,  newY) ;place beacon
					Sleep SleepTime
				}
			row++
			}
		}
	}
}

return