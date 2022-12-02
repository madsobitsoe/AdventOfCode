global _start

BITS 64
_start:
        mov r9, 1
        pop rdi               ; put no. of arguments in rdi
        cmp rdi, r9           ; Were there 1 argument only, i.e. the program name?
        je exitSuccess        ; if there were no args, exit
        pop rax               ; else pop arg-pointer into rax
        pop rax               ; twice so we get to the actual file supplied
        call openFile         ; Attempt to open the supplied file
        push rax              ; store fd on stack
        mov rdi, 5            ; fstat syscall
        xchg rdi,rax          ; syscall goes in rax, fd goes in rdi
        sub rsp, 0x100        ; make space for stat struct
        mov rsi, rsp          ; buf to read fstat-struct into (use stack)
        syscall
        cmp rax, 0
        jl exitError
        mov rcx, [rsp+48]       ; filesize returned from fstat-syscall
        add rsp, 0x100          ; get rsp back to before the stat-struct was written
        pop rax                 ; get fd
        push rcx                ; store num on stack for later
        push rax                ; store the fd on stack for later
        call readFileIntoMemory
        cmp rax, 0
        jl exitError            ;exit if error reading file
        mov [fileContentsAddr], rax ; store the address in .bss
        pop rax                 ; get fd
        call closeFile         ;; close the file

        mov rax, [fileContentsAddr]
        pop rcx                 ; size of data in rcx
        push rcx
	mov qword [totalPointsPart1], 0
	mov qword [entriesHandledPart1], 0
	mov qword [totalPointsPart2], 0
	mov qword [entriesHandledPart2], 0
	call handleEntriesPart1
	mov rax, [fileContentsAddr]
	pop rcx
	push rcx
	call handleEntriesPart2	
	mov rax, [totalPointsPart1]
        call printResult
	mov rax, [totalPointsPart2]
        call printResult

        mov rax, [fileContentsAddr]
        pop rcx                 ; num of bytes to munmap
        call unmapMem
        call exitSuccess

	
handleEntriesPart1:
        ;; Preconds:
        ;; addr in rax
        ;; length of data in rcx
        ;; postconds:
        ;; totalPoints updated with result
	push rax
	push rcx
handleEntriesLoopPart1:
	call handleEntryPart1
	inc qword [entriesHandledPart1]
	pop rcx
	sub rcx, 4
	push rcx
	cmp rcx, 0
	jg handleEntriesLoopPart1
	pop rcx
	pop rax
	ret

handleEntryPart1:
        ;; preconds:
        ;; addr of start of entry in rax
        ;; postconds:
	;; updates totalPointsPart1
        ;; returns address of next entry in rax
	;; rock,     paper,  scissors
	;; A = 0x41, B=0x42, C=0x43
	;; X = 0x58, Y=0x59, Z=0x5A
	xor edi, edi
	xor edx, edx
	movzx edi,  byte [rax] 	; opponent choice
	sub edi, 0x40 		; rock=1, paper=2,scissors=3
	add rax, 2
	movzx edx, byte [rax] 	; "my" choice
	sub edx, 0x57 		;rock=1, paper=2, scissors=3
	add qword [totalPointsPart1], rdx 	; add the player choice to the score
	add rax, 2			;
	cmp edi, edx
	jne notDraw
	add qword [totalPointsPart1], 3
	ret
notDraw:
	dec edi
	test edi, edi
	jz enemyRock
	dec edi
	test edi, edi
	jz enemyPaper
enemyScissors:
	;; we either played rock or paper
	cmp edx, 1
	je playerWin
	ret
enemyRock:
	;;  We either played paper or scissors
	cmp edx, 2 		;paper
	je playerWin
	ret 			; no points for rock vs scissor 

enemyPaper:	
	cmp edx, 3 		; did we play scissors and win?
	je playerWin
	ret 			; else return immediately
playerWin:	
	add qword [totalPointsPart1], 6
	ret

	;;  ================================
	;;  Part 2 below
	;;  ================================
handleEntriesPart2:
        ;; Preconds:
        ;; addr in rax
        ;; length of data in rcx
        ;; postconds:
        ;; totalPoints updated with result
	push rax
	push rcx
handleEntriesLoopPart2:
	call handleEntryPart2
	inc qword [entriesHandledPart2]
	pop rcx
	sub rcx, 4
	push rcx
	cmp rcx, 0
	jg handleEntriesLoopPart2
	pop rcx
	pop rax
	ret

handleEntryPart2:
        ;; preconds:
        ;; addr of start of entry in rax
        ;; max number of bytes to read in rcx // should be 4 
        ;; postconds
	;; updates totalPoints
        ;; returns address of next entry in rax
	;; rock,     paper,  scissors
	;; A = 0x41, B=0x42, C=0x43
	;; X = 0x58, Y=0x59, Z=0x5A
	;; diff is 23, (88-65), 0x17
	xor edi, edi
	xor edx, edx
	movzx edi,  byte [rax] 	; opponent choice
	sub edi, 0x40 		; rock=1, paper=2,scissors=3
	add rax, 2
	movzx edx, byte [rax] 	; "my" choice
	sub edx, 0x58 		; lose=0, draw=1, win=2
	;; add qword [totalPoints], rdx 	; add the player choice to the score
	add rax, 2			;
	test edx, edx
	jz shouldLose
	dec edx
	jz shouldDraw
shouldWin:
	dec edi
	test edi, edi
	jz enemyRockWeWin
	dec edi
	test edi,edi
	jz enemyPaperWeWin
enemyScissorsWeWin:
	add qword [totalPointsPart2], 7 ; 1 point for rock + 6 for win
	ret
enemyRockWeWin:
	add qword [totalPointsPart2], 8 ; 2 for paper, 6 for win
	ret
enemyPaperWeWin:
	add qword [totalPointsPart2], 9 ; 3 for scissors, 6 for win
	ret
shouldLose:
	dec edi
	test edi,edi
	jz enemyRockWeLose
	dec edi
	test edi,edi
	jz enemyPaperWeLose
enemyScissorsWeLose:
	add qword [totalPointsPart2], 2 ; 2 for paper, 0 for loss
	ret
enemyRockWeLose:
	add qword [totalPointsPart2], 3 ; 3 for scissors, 0 for loss
	ret
enemyPaperWeLose:
	add qword [totalPointsPart2], 1 ; 1 for rock, 0 for loss
	ret
shouldDraw:
	dec edi
	test edi,edi
	jz drawRock
	dec edi
	test edi,edi
	jz drawPaper
drawScissors:
	add qword [totalPointsPart2], 6 ; 3 for scissors, 3 for draw
	ret
drawRock:
	add qword [totalPointsPart2], 4 ; 1 for rock, 3 for draw
	ret
drawPaper:
	add qword [totalPointsPart2], 5 ; 2 for paper, 3 for draw
	ret

	;;  ================================
	;;  Helpful functions
	;;  ================================
unmapMem:
        ;; precond for unmapMem
        ;; address should be in rax
        ;; length/space of memory should be in rcx
        ;; Postcond: on success, mem is unmapped
        ;; on error, exits with errno from syscall
        ;; Remarks - uses syscall, so rax, rdi, rsi, r8, r9, r10 might be overwritten
        mov rdi, 11             ;syscall no for munmap
        xchg rax,rdi            ; *addr in rdi,syscall no. in rax
        mov rsi,rcx             ; num of bytes to unmap
        syscall                 ; unmap the mem-location
        cmp rax, 0              ; Error checking
        jne exitError
        ret                     ; Return from routine


openFile:
        ;; Precond for openfile
        ;; pointer to filename should be in rax
        ;; Postcond: Filedescriptor will be returned in rax
        ;; Remarks
        ;; Only readonly flag is set (NO WRITING YET!)
        ;; No mode is set (I don't need that yet)
        ;; Uses syscall, so rax, rsi, rdi, r8, r9, r10 might be overwritten
        mov rdi, rax            ; pointer to filename goes in rdi
        mov rax, 2              ; syscall for open
        xor rsi,rsi             ; Set Readonly-mode-flag
        xor rdx,rdx             ; Cancel the mode
        syscall
        ;; Handle errors!
        cmp rax, 0
        jl exitError           ;Something went wrong. Exit gracefully with errno
        ret                     ; return to calling function (lol, label). fd is in rax



readFileIntoMemory:
        ;; Preconds
        ;; fd to open should be in rax
        ;; no. of bytes to allocate should be in rcx
        ;; postconds
        ;; On success, address is returned in rax
        ;; on error, exits with errno from syscall
        ;; remarks:
        ;; Overwrites rax,rdi,rdx, r8,r9,r10
        ;; Reserves memory in readonly mode
        ;; Mem is backed by a file
        mov r8, 9               ;syscall for mmap
        xchg rax, r8            ;put fd in r8, syscall num in rax
        mov rsi, rcx            ; no. of bytes needed
        xor rdi,rdi             ;we don't care where the memory is. let the kernel decide
        ;; mov rsi, 20694          ;We need 20694 bytes to store the file
        mov rdx, 1              ; PROT_READ  - no need to overwrite the file
        mov r10, 2              ; MAP_PRIVATE
        xor r9, r9              ; no offset!
        syscall                 ; map the file and pray!
        cmp rax, 0              ; check for errors
        jl exitError            ; exit with errno from syscal
        ret



closeFile:
        ;; Preconds:
        ;; fd should be in rax
        ;; postcond:
        ;; fd is closed
        ;; remarks:
        ;; Potentially overwrites rax,rdi,rdx,r8,r9,r10
        mov rdi, 3              ;syscall for close
        xchg rax,rdi            ; put fd in rdi, syscall no. in rax
        syscall
        cmp rax,0               ;Check for errors
        jl exitError            ;exit with errno from syscall if error
        ret                     ; else, return


asciiToInt:
        ;; Preconds:
        ;; Addr of ascii-string in rax
        ;; length of ascii-string in rcx
        ;; postconds:
        ;; On success, result will be in rax
        ;; On error: returns -1 in rax
        ;; remarks: Overwrites rax,rcx,rdx,rsi,rdi

        ;; Use the stack for different vars
        ;; addr, bytes left
        push 0                  ; zero out new stackspace
        mov [rsp],cl          ; put bytes-to-read on stack
        mov rcx,rax            ; put addr in rcx
        xor rax,rax              ; rax is acc, 0 it out
        xor rsi,rsi              ; ensure rsi is 0'ed out
        mov rdi, 10              ; 10 for multiplying
        cmp byte [rsp], 0               ; assert length is longer than 0
        je asciiToIntError       ;if not positive length, return with error
asciiToIntLoopCheck:
        cmp byte [rsp], 0
        jg asciiToIntLoop
        add rsp,8              ;restore rsp
        ret                     ;If done, return
asciiToIntLoop:
        mul rdi             ; multiply acc by 10 (prepare for adding next digit)
        xor rsi, rsi            ; ensure rsi is all zeroes
        mov sil, [rcx]           ; read next byte
        cmp sil, 48              ; ensure gte '0' in ascii
        jl asciiToIntError
        cmp sil, 57              ; ensure lte '9' in ascii
        jg asciiToIntError
        sub sil, 48              ;convert to "int"
        add rax, rsi             ; add the read digit to rax
        dec byte [rsp]           ; decrement count of bytes left
        lea rcx, [rcx+1]          ; increment pointer to next byte
        jmp asciiToIntLoopCheck

asciiToIntError:
        mov rax, -1
        add rsp, 8             ;restore rsp
        ret


printResult:
        ;; Prints the result stored at [totalPoints]
        ;; Preconds:
        ;; result has been computed
        ;; mov rax, [totalPoints]
        sub rsp, 32 ; make space on stack for string
        ;; We have to add the char-bytes "backwards"
        ;; Add "Result: " and clean up stack
        mov dword [rsp], 0x75736552 ; "useR"
        mov dword [rsp+4], 0x203a746c ; " :tl"
        mov dword [rsp+8], 0
        mov qword [rsp+16], 0
        mov qword [rsp+24], 0
        xor rcx,rcx             ; use rcx for counting length of string
        mov rsi, 10             ; needed for division
intToAsciiLoop:
        xor rdx,rdx             ; clear before divide
        div rsi                 ; divide rdx:rax by 10. quotient updated in rax, remainder in rdx
        add edx, 48             ; convert remainder to ascii-digit
        inc ecx                 ; inc how many digits we've converted
        mov edi, 31             ; calculate offset for digit in string
        sub rdi, rcx            ; calculate offset for digit in string
        mov byte [rsp+rdi], dl  ; write digit to stack (from "behind")
        test rax,rax            ; are we done?
        jnz intToAsciiLoop      ; if not, get next digit
intToAsciiDone:
        mov byte [rsp+31], 10   ; add newline at end of string
        mov rax, 1              ; syscall for write
        mov rdi, 1              ; stdout
        mov rsi,rsp             ; print from stack
        mov rdx, 32             ; 32 bytes
        syscall                 ; WRITE
        add rsp, 32             ; restore stackpointer
        ret                     ; return

exitError:
        mov rdi, rax            ;errno is in rax
        neg rdi                 ;negate it to get an actual errcode
        and rdi,4096            ;and it with 4096 to get proper errcode
        mov rdi,42
        jmp exit

exitSuccess:
        mov rdi,0; return value for exit syscall
exit:
        mov     rax, 60         ; syscall for exit
        syscall                 ; exit successfully


        ;; section .data
section .bss
;; reserve 8 bytes for the memory address of the file we read
fileContentsAddr:       resb 8
;; Reserve 8 bytes for accumulated score
totalPointsPart1:	 resb 8
entriesHandledPart1:	 resb 8
totalPointsPart2:	 resb 8
entriesHandledPart2:	 resb 8
