	.intel_syntax noprefix
	.globl _main
_main:
	push rbp
	mov rbp, rsp
	sub rsp, 8
	mov DWORD PTR [rbp - 4], 3
	mov r10d, DWORD PTR [rbp - 4]
	mov DWORD PTR [rbp - 8], r10d
	add DWORD PTR [rbp - 8], 5
	mov r10d, DWORD PTR [rbp - 8]
	mov DWORD PTR [rbp - 4], r10d
