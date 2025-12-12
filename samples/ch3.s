	.intel_syntax noprefix
	.globl _main
_main:
	push rbp
	mov rbp, rsp
	sub rsp, 16
	mov DWORD PTR [rbp - 4], 1
	imul 2 DWORD PTR [rbp - 4]
	mov DWORD PTR [rbp - 8], 4
	add 5 DWORD PTR [rbp - 8]
	mov DWORD PTR [rbp - 12], 3
	mov r11, DWORD PTR [rbp - 12]
	imul DWORD PTR [rbp - 8] r11
	mov DWORD PTR [rbp - 12], r11
	mov r10d, DWORD PTR [rbp - 4]
	mov DWORD PTR [rbp - 16], r10d
	sub DWORD PTR [rbp - 12] DWORD PTR [rbp - 16]
	mov eax, DWORD PTR [rbp - 16]
	mov rsp, rbp
	pop rbp
	ret
