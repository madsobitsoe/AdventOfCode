BITS 64
  org 0x400000

ehdr:           ; Elf64_Ehdr
  db 0x7f, "ELF", 2, 1, 1, 0 ; e_ident
  times 8 db 0
  dw  2         ; e_type
  dw  0x3e      ; e_machine
  dd  1         ; e_version
  dq  _start    ; e_entry
  dq  phdr - $$ ; e_phoff
  dq  0         ; e_shoff
  dd  0         ; e_flags
  dw  ehdrsize  ; e_ehsize
  dw  phdrsize  ; e_phentsize
  dw  1         ; e_phnum
  dw  0         ; e_shentsize
  dw  0         ; e_shnum
  dw  0         ; e_shstrndx
  ehdrsize  equ  $ - ehdr

phdr:           ; Elf64_Phdr
  dd  1         ; p_type
  dd  5         ; p_flags
  dq  0         ; p_offset
  dq  $$        ; p_vaddr
  dq  $$        ; p_paddr
  dq  filesize  ; p_filesz
  dq  filesize  ; p_memsz
  dq  0x1000    ; p_align
  phdrsize  equ  $ - phdr

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
        push rcx                ; store num of bytes read on stack for later
        push rax                ; store the fd on stack for later
        call readFileIntoMemory
        cmp rax, 0
        jl exitError            ;exit if error reading file
	mov r10, rax  ; store the address in r10
        pop rax                 ; get fd
        call closeFile         ;; close the file

	mov rax, r10 		; fileContentsAddr
        pop rcx                 ; size of data in rcx
        push rcx
	mov r11, 0 		; part 1 sum
	call handleEntries
	mov rax, r11
        call printResult
	mov rax, r10 		; fileContentsAddr
        pop rcx                 ; num of bytes to munmap
        call unmapMem
        call exitSuccess

	
handleEntries:
        ;; Preconds:
        ;; addr of read data in rax
        ;; length of data in rcx
        ;; postconds:
        ;; r11 (sum) holds result
	mov rbp, rsp
	sub rsp, 0x100
	push rax
	push rcx
handleEntriesLoop:
	call handleEntry
	cmp rcx, 0
	jg handleEntriesLoop
	pop rcx
	pop rax
	mov rsp, rbp
	ret

handleEntry:
        ;; preconds:
        ;; addr of start of entry in rax
	;; number of bytes to read in rcx
        ;; postconds:
	;; updates sum in r11
        ;; returns address of next entry in rax
	xor edi, edi
	xor edx, edx
.nextchar:
	movzx edi,  byte [rax] 	; read next char
	inc rax 		; next char
	dec ecx			; did read one char
	cmp di, 0x30 		; '0'
	jl .nextchar
	cmp di, 0x39 		; '9'
	jg .nextchar
	xchg edi, edx
.findnewline:
	movzx edi, byte [rax]
	inc rax
	dec rcx
	cmp di, 0xa
	jne .findnewline
	mov rbx, rax 		; store address of newline for next iteration
.charfrombehind:
	dec rax
	movzx edi, byte [rax]
	cmp di, 0x30
	jl .charfrombehind
	cmp di, 0x39
	jg .charfrombehind
	shl edi, 8
	or edi, edx
	push rcx
	push rdi
	mov rax, rsp
	mov ecx, 2
	call asciiToInt
	add r11, rax
	mov rax, rbx 		; restore pointer in rax
	pop rcx
	pop rcx

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
        ;; Preconds:
        ;; result as int in rax
        sub rsp, 32 ; make space on stack for string
        ;; We have to add the char-bytes "backwards"
        ;; Add "Result: " and clean up stack
        mov dword [rsp], 0x75736552 ; "useR"
        mov dword [rsp+4], 0x203a746c ; " :tl"
        mov dword [rsp+8], 0
        mov qword [rsp+16], 0
        mov qword [rsp+24], 0
        xor ecx,ecx             ; use rcx for counting length of string
        mov rsi, 10             ; needed for division
intToAsciiLoop:
        xor edx,edx             ; clear before divide
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
        mov eax, 1              ; syscall for write
        mov edi, 1              ; stdout
        mov rsi,rsp             ; print from stack
        mov edx, 32             ; 32 bytes
        syscall                 ; WRITE
        add rsp, 32             ; restore stackpointer
        ret                     ; return

exitError:
        mov rdi, rax            ;errno is in rax
        neg rdi                 ;negate it to get an actual errcode
        and rdi,4096            ;and it with 4096 to get proper errcode
        jmp exit

exitSuccess:
        xor edi, edi 	    	; return value for exit syscall
exit:
        mov eax, 60         ; syscall for exit
        syscall                 ; exit successfully

filesize  equ  $ - $$
