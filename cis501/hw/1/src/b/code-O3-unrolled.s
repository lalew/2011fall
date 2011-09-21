	.file	"code.c"
	.text
	.p2align 4,,15
.globl saxpy
	.type	saxpy, @function
saxpy:
.LFB0:
	.cfi_startproc
	movss	a(%rip), %xmm0
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
.L2:
	leaq	4(%rax), %r10
	leaq	8(%rax), %r9
	leaq	12(%rax), %r8
	leaq	16(%rax), %rdi
	leaq	20(%rax), %rsi
	leaq	24(%rax), %rcx
	leaq	28(%rax), %rdx
	movss	x(%rax), %xmm8
	mulss	%xmm0, %xmm8
	movss	x(%r10), %xmm7
	movss	x(%r9), %xmm6
	mulss	%xmm0, %xmm7
	movss	x(%r8), %xmm5
	mulss	%xmm0, %xmm6
	movss	x(%rdi), %xmm4
	mulss	%xmm0, %xmm5
	movss	x(%rsi), %xmm3
	mulss	%xmm0, %xmm4
	movss	x(%rcx), %xmm2
	mulss	%xmm0, %xmm3
	movss	x(%rdx), %xmm1
	mulss	%xmm0, %xmm2
	mulss	%xmm0, %xmm1
	addss	y(%rax), %xmm8
	addss	y(%r10), %xmm7
	addss	y(%r9), %xmm6
	addss	y(%r8), %xmm5
	addss	y(%rdi), %xmm4
	addss	y(%rsi), %xmm3
	addss	y(%rcx), %xmm2
	movss	%xmm8, z(%rax)
	addq	$32, %rax
	addss	y(%rdx), %xmm1
	cmpq	$262144, %rax
	movss	%xmm7, z(%r10)
	movss	%xmm6, z(%r9)
	movss	%xmm5, z(%r8)
	movss	%xmm4, z(%rdi)
	movss	%xmm3, z(%rsi)
	movss	%xmm2, z(%rcx)
	movss	%xmm1, z(%rdx)
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
