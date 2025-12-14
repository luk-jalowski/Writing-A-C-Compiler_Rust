	.intel_syntax noprefix
	.globl _main
_main:
	push rbp
	mov rbp, rsp
	sub rsp, 8
	mov eax, 1
	cdq
	mov r10d, 0
	idiv r10d
	mov DWORD PTR [rbp - 4], eax
	mov r11d, 0
	cmp r11d, 0
je .L0
	cmp DWORD PTR [rbp - 4], 0
je .L0
	mov DWORD PTR [rbp - 8], 1
jmp .L1
.L0:
	mov DWORD PTR [rbp - 8], 0
.L1:
	mov eax, DWORD PTR [rbp - 8]
	mov rsp, rbp
	pop rbp
	ret
