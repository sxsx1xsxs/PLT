	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 16, 6
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rax
Ltmp0:
	.cfi_def_cfa_offset 16
	movq	(%rsp), %rsi
	leaq	L_fmt.1(%rip), %rdi
	xorl	%eax, %eax
	callq	_printf
	xorl	%eax, %eax
	popq	%rdx
	retq
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_fmt:                                  ## @fmt
	.asciz	"%s\n"

L_fmt.1:                                ## @fmt.1
	.asciz	"%s\n"

L_fmt.2:                                ## @fmt.2
	.asciz	"%d\n"

L_fmt.3:                                ## @fmt.3
	.asciz	"f\n"


.subsections_via_symbols
