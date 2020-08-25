INITSEG  = 0x9000	! we move boot here - out of the way
SYSSEG   = 0x1000	! system loaded at 0x10000 (65536).
SETUPSEG = 0x9020	! this is the current segment

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

entry start
start:

!---------------------------实验内容开始----------------------
print_meg:
    mov ah,#0x03
    xor bh,bh
    int 0x10
    mov cx,#25
    mov bx,#0x0007
    mov bp,#msg2
    mov ax,cs
    mov es,ax
    mov ax,#0x1301
    int 0x10

print_cusor:
    mov ah,#0x03
    xor bh,bh
    int 0x10 ! return在dx中
    mov cx,#18
    mov bx,#0x0007
    mov bp,#msg_cursor
    mov ax,#0x1301
    int 0x10
    call print_hex

print_memory:

    mov ah,#0x03
    xor bh,bh
    int 0x10 !

    mov cx,#14
    mov bx,#0x0007
    mov bp,#msg_memory
    mov ax,#0x1301
    int 0x10

    mov	ah,#0x88 !获取扩展内存
	int	0x15
	mov dx,ax ! return 在ax上
    call print_hex

print_KB:
	!显示扩展内存最后的单位"KB"
	mov	ah,#0x03		! read cursor pos
	xor	bh,bh			! 页号bh=0
	int	0x10

	mov	cx,#2
	mov	bx,#0x0007		! page 0, attribute 7 (normal) 页号BH=0 属性BL=7正常显示
	mov	bp,#msg_kb		! ES:BP要显示的字符串地址
	mov	ax,#0x1301		! write string, move cursor AH=13显示字符串 AL=01光标跟随移动
	int	0x10



    call print_nl
!-----------------实验内容结束----------------


! ok, the read went well so we get current cursor position and save it for
! posterity.
	mov	ax,#INITSEG	! thiprint_hexs is done in bootsect already, but...
	mov	ds,ax
	mov	ah,#0x03	! read 光标的位置
	xor	bh,bh
	int	0x10		! save it in known place, con_init fetches
	mov	[0],dx		! it from 0x90000.

! Get memory size (extended mem, kB)

	mov	ah,#0x88 !获取扩展内存
	int	0x15
	mov	[2],ax ! 间接寻址 cs<<4+2 -> 90004

! Get video-card data:

	mov	ah,#0x0f !当前的显示模式
	int	0x10
	mov	[4],bx		! bh = display page
	mov	[6],ax		! al = video mode, ah = window width

! check for EGA/VGA and some config parameters

	mov	ah,#0x12
	mov	bl,#0x10
	int	0x10
	mov	[8],ax
	mov	[10],bx
	mov	[12],cx

! Get hd0 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x41]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0080
	mov	cx,#0x10
	rep
	movsb

! Get hd1 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x46]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	rep
	movsb

! Check that there IS a hd1 :-)

	mov	ax,#0x01500
	mov	dl,#0x81
	int	0x13
	jc	no_disk1
	cmp	ah,#3
	je	is_disk1
no_disk1:
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	mov	ax,#0x00
	rep
	stosb
is_disk1:

! now we want to move to protected mode ...

	cli			! no interrupts allowed !

! first we move the system to it's rightful place

	mov	ax,#0x0000
	cld			! 'direction'=0, movs moves forward
do_move:
	mov	es,ax		! destination segment
	add	ax,#0x1000  ! 0x1000 开始
	cmp	ax,#0x9000  ! 比较等于0x9000 结束
	jz	end_move
	mov	ds,ax		! source segment 0x1000:0x0
	sub	di,di
	sub	si,si
	mov 	cx,#0x8000 !一次移动64K数据
	rep
	movsw !将DS：SI的内容复制到ES：DI 0x1000:0x0->0x0000:0x0 ,一共移动了8次 一共512k
	jmp	do_move

! then we load the segment descriptors

end_move:
	mov	ax,#SETUPSEG	! right, forgot this at first. didn't work :-)
	mov	ds,ax
	lidt	idt_48 ! 加载idt 中断描述符表
	lgdt	gdt_48		! 加载gdt 全局描述符表

! that was painless, now we enable A20

	call	empty_8042
	mov	al,#0xD1		! command write
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF		! A20 on
	out	#0x60,al
	call	empty_8042

! well, that went ok, I hope. Now we have to reprogram the interrupts :-(
! we put them right after the intel-reserved hardware interrupts, at
! int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
! messed this up with the original PC, and they haven't been able to
! rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
! which is used for the internal hardware interrupts as well. We just
! have to reprogram the 8259's, and it isn't fun. 初始话中断

	mov	al,#0x11		! initialization sequence
	out	#0x20,al		! send it to 8259A-1 发送指令
	.word	0x00eb,0x00eb		! jmp $+2, jmp $+2 起延时作用
	out	#0xA0,al		! and to 8259A-2
	.word	0x00eb,0x00eb
	mov	al,#0x20		! start of hardware int's (0x20)
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		! start of hardware int's 2 (0x28)
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		! 8259-1 is master
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		! 8259-2 is slave
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		! 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		! mask off all interrupts for now
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.
	mov	ax,#0x0001	! protected mode (PE) bit
	lmsw	ax		! This is it!
	jmpi	0,8		! setup最后一条指令,寻址方式改变 ip:0 cs:8
	! 开始选址 执行 cs 选择址32, ip也是32位

! This routine checks that the keyboard command queue is empty
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.
empty_8042:
	.word	0x00eb,0x00eb
	in	al,#0x64	! 8042 status port
	test	al,#2		! is input buffer full?
	jnz	empty_8042	! yes - loop
	ret

!通过bios中断0x10打印出 dx内容 以16进制 显示

print_hex:
    mov    cx,#4 !设定 循环4次 高4位开始打印4次

print_loop:
    rol    dx,#4 !循环左移4位, 假设 dx的二进制为111100001111000 --rol-->0000111100001111
    mov    ax,#0xe07 !0x10号中断，功能号ah=0x0e表示显示al所对应ascii码的字符到屏幕上
    and    al,dl ! al的低8位07(00001111) & dl,  去掉5-8位,保留1-4位
    add    al,#0x30  !ascii 数字从0x30(48)开始,
    cmp    al,#0x3a   ! 判断是否是数字. 16进制的字母 a b c d e f 显示要+7
    jl     outp !有符号小于跳转
    add    al,#0x07 !否则+7表示为一个字母 因为字母a~f的ascii码为 0x41~0x46

outp:
    int    0x10
    loop   print_loop
    ret

!打印回车换行
print_nl:
	mov ax, #0xe0d
	int 0x10
	mov al, #0xa
	int 0x10
	ret


gdt:
	.word	0,0,0,0		! dummy  8字节 4字为一个单元

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb) // 0x8
	.word	0x0000		! base address=0
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb) // 0x10
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L

gdt_48:
	.word	0x800		! gdt limit=2048, 256 GDT entries
	.word	512+gdt,0x9	! gdt base = 0X9xxxx


msg2:
	.byte 13,10
	.ascii "NOW we are in SETUP"
	.byte 13,10,13,10

msg_cursor:
    .byte 13,10
    .ascii "Cursor position:"
msg_memory:
    .byte 13,10
    .ascii "Memory Size:"
msg_cyles:
    .byte 13,10
    .ascii "Cyls:"
msg_heads:
    .byte 13,10
    .ascii "Heads:"
msg_kb:
    .ascii "KB"



.text
endtext:
.data
enddata:
.bss
endbss:
