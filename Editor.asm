; Text Editor - Fixed F2 Load to Allow Full Editing
; Fix: Populate LineStack when loading files so backspace works on loaded content

.model small
.stack 100h

; --- Macro for printing strings ---
PRINT_STRING MACRO MSG_ADDR
    PUSH DX
    PUSH AX
    MOV AH, 09h
    LEA DX, MSG_ADDR
    INT 21h
    POP AX
    POP DX
ENDM

.data
    ; --- UI Messages ---
    intro      db "Text Editor (F1: Save, F2: Load, Esc: Close)", 0dh, 0ah, "--------------------------------",0dh, 0ah,'$'
    funcs      db "[F1]: Save output.txt, [F2]: Load input.txt", 0dh, 0ah, "--------------------------------", 0dh, 0ah, '$'
    
    ; --- File Messages ---
    fileSaving db 0dh, 0ah, "Saving...", 0dh, 0ah, '$'
    fileSaved  db 0dh, 0ah, "Saved OK!", 0dh, 0ah, '$'
    fileLoading db 0dh, 0ah, "Loading...", 0dh, 0ah, '$'
    fileLoaded db 0dh, 0ah, "Loaded OK! You can edit now.", 0dh, 0ah, '$'
    fileError  db 0dh, 0ah, "File Error!", 0dh, 0ah, '$'
    
    ; --- File Variables ---
    inputFile  db 'input.txt', 0
    outputFile db 'output.txt', 0
    fileHandle DW ?
    fileLength DW 0        ; length in bytes currently in buffer

    ; --- Editor State Variables ---
    buffer db 60000 dup(?) 
    LineStack db 100 DUP(?)  
    StackTop db 0            
    CurrentLineLen db 0     

.code

main proc far
    .startup
        LEA SI, buffer
        XOR CX, CX
        MOV fileLength, CX    ; initially zero
        call start

    InputLoop:
        MOV AH, 00h
        INT 16h

        ; --- Check for Special Keys ---
        CMP AL, 00h
        JNE CheckASCII

        ; --- Function keys in AH when AL=0 ---
        CMP AH, 3Bh    ; F1
        JE F1True
        
        CMP AH, 3Ch    ; F2
        JNE CheckFunctionKeysEnd
        JMP F2True

    CheckFunctionKeysEnd:
        JMP InputLoop

    CheckASCII:
        CMP AL, 0Dh ; Enter
        JNE CheckEsc
        JMP EnterTrue

    CheckEsc:
        CMP AL, 1Bh ; Esc
        JE EscTrue
        
        CMP AL, 08h ; Backspace
        JNE CheckNorm
        JMP BackSpaceTrue

    CheckNorm:
        JMP Norm

    EscTrue:
        .exit

    ; =================================================================
    ; == F1: Save Text Function
    ; =================================================================
    F1True:
        CMP fileLength, 0
        JE InputLoop ; nothing to save

        PUSH SI
        PUSH CX

        PRINT_STRING fileSaving

        ; Create File
        MOV AH, 3Ch
        MOV CX, 0
        LEA DX, outputFile
        INT 21h
        JC F1_Error_Restore

        MOV [fileHandle], AX

        ; Prepare write: CX = length
        MOV CX, [fileLength]
        MOV AH, 40h
        MOV BX, [fileHandle]
        LEA DX, buffer
        INT 21h
        JC F1_Error_Restore

        ; Check bytes written === requested
        CMP AX, CX
        JNE F1_Partial_Write

    F1_Write_OK:
        ; Close File
        MOV AH, 3Eh
        MOV BX, [fileHandle]
        INT 21h

        ; Restore Editor State
        POP CX
        POP SI

        ; Ensure SI points to end for refresh
        MOV SI, OFFSET buffer
        ADD SI, [fileLength]

        CALL RefreshScreen
        PRINT_STRING fileSaved
        JMP InputLoop

    F1_Partial_Write:
        MOV AH, 3Eh
        MOV BX, [fileHandle]
        INT 21h
        POP CX
        POP SI
        PRINT_STRING fileError
        JMP InputLoop

    F1_Error_Restore:
        POP CX
        POP SI
        PRINT_STRING fileError
        JMP InputLoop

    ; =================================================================
    ; == F2: Load Text Function (FIXED - Now Fully Editable)
    ; =================================================================
    F2True:
        ; 1. Open File
        MOV AH, 3Dh
        MOV AL, 00h
        LEA DX, inputFile
        INT 21h
        JC F2_Error_Simple

        MOV [fileHandle], AX

        ; 2. Read Data
        MOV AH, 3Fh
        MOV BX, [fileHandle]
        LEA DX, buffer
        MOV CX, 60000
        INT 21h
        JC F2_Error_Simple

        ; AX = bytes read
        PUSH AX

        ; 3. Close File
        MOV AH, 3Eh
        MOV BX, [fileHandle]
        INT 21h

        POP AX
        MOV CX, AX
        MOV [fileLength], CX

        ; ---------------------------------------------------------
        ; NEW: Build LineStack from loaded content
        ; ---------------------------------------------------------
        MOV StackTop, 0          ; Reset stack
        MOV CurrentLineLen, 0    ; Reset current line length
        
        ; If file is empty, skip scanning
        CMP CX, 0
        JE F2_SkipScan
        
        LEA SI, buffer           ; Start from beginning
        XOR DX, DX               ; DX = current line char count
        
    F2_ScanLoop:
        CMP SI, OFFSET buffer
        JB F2_ScanDone
        
        ; Calculate remaining bytes
        LEA DI, buffer
        ADD DI, CX               ; DI = end of buffer
        CMP SI, DI
        JAE F2_ScanDone          ; Reached end
        
        MOV AL, [SI]
        INC SI
        
        ; Check if this is a newline (LF)
        CMP AL, 0Ah
        JE F2_FoundNewline
        
        ; Regular character - increment line length
        INC DX
        JMP F2_ScanLoop
        
    F2_FoundNewline:
        ; We found a complete line - push its length to stack
        MOV BL, StackTop
        CMP BL, 100              ; Check stack overflow
        JAE F2_StackFull
        
        XOR BH, BH
        MOV LineStack[BX], DL    ; Store line length
        INC StackTop
        
        XOR DX, DX               ; Reset line length counter
        JMP F2_ScanLoop
        
    F2_StackFull:
        ; Stack full - just continue without tracking more lines
        JMP F2_ScanLoop
        
    F2_ScanDone:
        ; DX now contains the length of the last line (after last newline or entire file if no newlines)
        MOV CurrentLineLen, DL

    F2_SkipScan:
        ; Set SI to end of buffer (offset + length)
        LEA SI, buffer
        ADD SI, CX

        ; Refresh Screen
        CALL RefreshScreen

        JMP InputLoop

    F2_Error_Simple:
        PRINT_STRING fileError
        JMP InputLoop

    ; =================================================================
    ; == Editing Logic
    ; =================================================================
    EnterTrue:
        ; push current line length on LineStack
        MOV BL, StackTop
        CMP BL, 100              ; Check stack overflow
        JAE EnterStackFull
        
        XOR BH, BH
        MOV AL, CurrentLineLen
        MOV LineStack[BX], AL
        INC StackTop

    EnterStackFull:
        MOV CurrentLineLen, 0

        ; append CR LF
        MOV BYTE PTR [SI], 0Dh
        INC SI
        MOV BYTE PTR [SI], 0Ah
        INC SI

        ; update lengths
        ADD WORD PTR [fileLength], 2
        MOV CX, [fileLength]

        ; print CR LF
        MOV AH, 02h
        MOV DL, 0Dh
        INT 21h
        MOV DL, 0Ah
        INT 21h

        JMP InputLoop

    Norm:
        ; store char in buffer and increment counters
        MOV [SI], AL
        INC SI
        INC WORD PTR [fileLength]
        MOV CX, [fileLength]
        INC CurrentLineLen

        ; echo char
        MOV AH, 02h
        MOV DL, AL
        INT 21h
        JMP InputLoop

    BackSpaceTrue:
        ; if nothing to delete, do nothing
        CMP [fileLength], 0
        
        JNE CON
        jmp InputLoop
    CON:
        DEC SI
        DEC WORD PTR [fileLength]

        MOV AL, [SI]
        CMP AL, 0Ah
        JE DeleteEnter ; if it's LF (part of CR/LF), handle specially

        DEC CurrentLineLen

        ; erase char visually
        MOV AH, 02h
        MOV DL, 08h
        INT 21h
        MOV DL, ' '
        INT 21h
        MOV DL, 08h
        INT 21h
        MOV CX, [fileLength]
        JMP InputLoop

    DeleteEnter:
        ; Check if StackTop is 0 - now this should rarely happen since we populate stack on load
        CMP StackTop, 0
        JE DeleteEnter_Abort

        ; Delete CR/LF pair
        DEC SI
        DEC WORD PTR [fileLength]

        DEC StackTop
        MOV BL, StackTop
        XOR BH, BH
        MOV AL, LineStack[BX]
        MOV CurrentLineLen, AL

        ; Refresh screen to show the joined lines
        PUSH SI
        CALL RefreshScreen
        POP SI

        ; move cursor to correct position on previous line
        MOV AH, 03h
        MOV BH, 0
        INT 10h
        ; DH = row, DL = col
        CMP DH, 0
        JE SkipCursorUp
        DEC DH
    SkipCursorUp:
        MOV DL, CurrentLineLen
        MOV AH, 02h
        MOV BH, 0
        INT 10h

        MOV CX, [fileLength]
        JMP InputLoop

    DeleteEnter_Abort:
        ; Restore state
        INC SI
        INC WORD PTR [fileLength]
        MOV CX, [fileLength]
        JMP InputLoop

main endp

    ; =================================================================
    ; == Utility Procedures
    ; =================================================================
    start proc near
        CALL ClearScreen
        mov ah,09h
        LEA dx, intro
        int 21h
        LEA dx, funcs
        int 21h
        ret
    start endp

    ClearScreen proc near
        MOV AH, 06h
        MOV AL, 00h
        MOV BH, 07h
        MOV CX, 0000h
        MOV DX, 184Fh
        INT 10h

        MOV AH, 02h
        MOV BH, 0
        XOR DX, DX
        INT 10h
        ret
    ClearScreen endp

    RefreshScreen proc near
        CALL ClearScreen
        mov ah,09h
        LEA dx, intro
        int 21h
        LEA dx, funcs
        int 21h

        ; Put '$' at the end so DOS knows where to stop
        PUSH AX
        PUSH SI

        ; SI must point to end (offset + fileLength)
        MOV AX, [fileLength]
        LEA SI, buffer
        ADD SI, AX

        MOV BYTE PTR [SI], '$'

        MOV AH, 09h
        LEA DX, buffer
        INT 21h

        ; Remove '$' (clean up)
        MOV BYTE PTR [SI], ' '
        
        POP SI
        POP AX
        ret
    RefreshScreen endp

end main