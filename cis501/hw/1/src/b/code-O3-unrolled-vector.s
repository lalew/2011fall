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
	shufps	$0, %xmm0, %xmm0
	.p2align 4,,10
	.p2align 3
.L2:
	leaq	16(%rax), %r10
	leaq	32(%rax), %r9
	leaq	48(%rax), %r8
	leaq	64(%rax), %rdi
	leaq	80(%rax), %rsi
	leaq	96(%rax), %rcx
	leaq	112(%rax), %rdx
	movaps	x(%rax), %xmm8
	movaps	x(%r10), %xmm7
	mulps	%xmm0, %xmm8
	movaps	x(%r9), %xmm6
	mulps	%xmm0, %xmm7
	movaps	x(%r8), %xmm5
	mulps	%xmm0, %xmm6
	movaps	x(%rdi), %xmm4
	mulps	%xmm0, %xmm5
	addps	y(%rax), %xmm8
	movaps	x(%rsi), %xmm3
	mulps	%xmm0, %xmm4
	addps	y(%r10), %xmm7
	movaps	x(%rcx), %xmm2
	mulps	%xmm0, %xmm3
	addps	y(%r9), %xmm6
	movaps	x(%rdx), %xmm1
	mulps	%xmm0, %xmm2
	addps	y(%r8), %xmm5
	movaps	%xmm8, z(%rax)
	mulps	%xmm0, %xmm1
	subq	$-128, %rax
	cmpq	$262144, %rax
	movaps	%xmm7, z(%r10)
	addps	y(%rdi), %xmm4
	movaps	%xmm6, z(%r9)
	movaps	%xmm5, z(%r8)
	addps	y(%rsi), %xmm3
	movaps	%xmm4, z(%rdi)
	addps	y(%rcx), %xmm2
	addps	y(%rdx), %xmm1
	movaps	%xmm3, z(%rsi)
	movaps	%xmm2, z(%rcx)
	movaps	%xmm1, z(%rdx)
	jne	.L2
	rep
	ret
	.cfi_endproc
.LFE0:
	.size	saxpy, .-saxpy
	.comm	x,262144,32
	.comm	y,262144,32
	.comm	z,262144,32
	.comm	a,4,16
	.ident	"GCC: (SUSE Linux) 4.5.1 20101208 [gcc-4_5-branch revision 167585]"
	.section	.comment.SUSE.OPTs,"MS",@progbits,1
	.string	"Ospwg"
	.section	.note.GNU-stack,"",@progbits
