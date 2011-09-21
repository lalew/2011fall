	.file	"code.c"
	.comm	x,262144,32
	.comm	y,262144,32
	.comm	z,262144,32
	.comm	a,4,4
	.text
.globl saxpy
	.type	saxpy, @function
saxpy:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	movq	%rsp, %rbp
	.cfi_offset 6, -16
	.cfi_def_cfa_register 6
	movl	$0, -4(%rbp)
	jmp	.L2
.L3:
	movl	-4(%rbp), %eax
	cltq
	movss	x(,%rax,4), %xmm1
	movss	a(%rip), %xmm0
	mulss	%xmm1, %xmm0
	movl	-4(%rbp), %eax
	cltq
	movss	y(,%rax,4), %xmm1
	addss	%xmm1, %xmm0
	movl	-4(%rbp), %eax
	cltq
	movss	%xmm0, z(,%rax,4)
	addl	$1, -4(%rbp)
.L2:
	cmpl	$65535, -4(%rbp)
	jle	.L3
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	saxpy, .-saxpy
	.ident	"GCC: (SUSE Linux) 4.5.1 20101208 [gcc-4_5-branch revision 167585]"
	.section	.comment.SUSE.OPTs,"MS",@progbits,1
	.string	"ospwg"
	.section	.note.GNU-stack,"",@progbits
