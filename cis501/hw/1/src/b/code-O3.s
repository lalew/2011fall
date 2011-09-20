	.section	__TEXT,__text,regular,pure_instructions
	.globl	_saxpy
	.align	4, 0x90
_saxpy:
Leh_func_begin1:
	pushq	%rbp
Ltmp0:
	movq	%rsp, %rbp
Ltmp1:
	movq	_a@GOTPCREL(%rip), %rax
	movss	(%rax), %xmm0
	xorl	%eax, %eax
	movq	_x@GOTPCREL(%rip), %rcx
	movq	_y@GOTPCREL(%rip), %rdx
	movq	_z@GOTPCREL(%rip), %rsi
	.align	4, 0x90
LBB1_1:
	movss	(%rcx,%rax,4), %xmm1
	mulss	%xmm0, %xmm1
	addss	(%rdx,%rax,4), %xmm1
	movss	%xmm1, (%rsi,%rax,4)
	incq	%rax
	cmpq	$65536, %rax
	jne	LBB1_1
	popq	%rbp
	ret
Leh_func_end1:

	.comm	_x,262144,5
	.comm	_a,4,2
	.comm	_y,262144,5
	.comm	_z,262144,5
	.section	__TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame0:
Lsection_eh_frame:
Leh_frame_common:
Lset0 = Leh_frame_common_end-Leh_frame_common_begin
	.long	Lset0
Leh_frame_common_begin:
	.long	0
	.byte	1
	.asciz	 "zR"
	.byte	1
	.byte	120
	.byte	16
	.byte	1
	.byte	16
	.byte	12
	.byte	7
	.byte	8
	.byte	144
	.byte	1
	.align	3
Leh_frame_common_end:
	.globl	_saxpy.eh
_saxpy.eh:
Lset1 = Leh_frame_end1-Leh_frame_begin1
	.long	Lset1
Leh_frame_begin1:
Lset2 = Leh_frame_begin1-Leh_frame_common
	.long	Lset2
Ltmp2:
	.quad	Leh_func_begin1-Ltmp2
Lset3 = Leh_func_end1-Leh_func_begin1
	.quad	Lset3
	.byte	0
	.byte	4
Lset4 = Ltmp0-Leh_func_begin1
	.long	Lset4
	.byte	14
	.byte	16
	.byte	134
	.byte	2
	.byte	4
Lset5 = Ltmp1-Ltmp0
	.long	Lset5
	.byte	13
	.byte	6
	.align	3
Leh_frame_end1:


.subsections_via_symbols
