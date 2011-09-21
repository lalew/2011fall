	.file	"code.c"
	.text
	.p2align 4,,15
.globl saxpy
	.type	saxpy, @function
saxpy:
.LFB0:
	.cfi_startproc
	movss	a(%rip), %xmm1
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
.L2:
	movss	x(%rax), %xmm0
	mulss	%xmm1, %xmm0
	addss	y(%rax), %xmm0
	movss	%xmm0, z(%rax)
	addq	$4, %rax
	cmpq	$262144, %rax
	jne	.L2
	rep
	ret
	.cfi_endproc
.LFE0:
	.size	saxpy, .-saxpy
	.comm	x,262144,32
	.comm	y,262144,32
	.comm	z,262144,32
	.comm	a,4,4
	.ident	"GCC: (SUSE Linux) 4.5.1 20101208 [gcc-4_5-branch revision 167585]"
	.section	.comment.SUSE.OPTs,"MS",@progbits,1
	.string	"Ospwg"
	.section	.note.GNU-stack,"",@progbits
