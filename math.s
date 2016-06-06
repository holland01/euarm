;		Counting theory:
;		Make sure that no multiple which already has been evaluated is added to the sum a second time.
;		Since every multiple of five increases at a rate which is faster than the the 3 iterator,
;		we can simply check the current multiple from the 3 iterator to see if it's also evenly divisible
;		by 5.

;		Figure out how to handle the fractional component for the division by five check:
;		remember, (2^16 / 5) ~= 13107.2 ~= 0x3333.33 . Figure the best means of using the 0.33
;		fraction in order to achieve this.

;		For example, we can represent the constant as 0x333300 + 0x0000FF. In order to get the fractional component,
;		we should be able to AND the entire value with the second term. However, since this is base 2 things may be a tad different.


.text

.align 4

.global _start

;		TODO
;       finish memset test
;       find out whether or not memory is word addressable or byte
;           addressable - if necessary, adjust accordingly.
;       add remainder support


;		division with signed values still needs to be implemented...

;============================
;		wordmask
;		returns 0xFFFFFFFF
;============================

wordmask:
    orr		r0,		r0,		#0x000000FF
    orr		r0,		r0,		#0x0000FF00
    orr		r0,		r0,		#0x00FF0000
    orr		r0,		r0,		#0xFF000000
    mov		pc,		lr

;============================
;		div
;		returns floor(a / b) in addition to the remainder, where
;		a -> r0
;		b -> r1
;		r0 returns the quotient
;		r1 returns the remainder
;============================

div:
    stmdb	r13!,	{r4, lr}
    mov		r4, 		#0x0			; initialize counter

div_loop:
    cmp		r0, 		r1			; since we're flooring the division, the moment that a < b is when we need to stop
    blt		div_stop				; ----------------------------------
    sub		r0,		r0,		r1	; a -= b
    add		r4,		r4,		#0x1	; counter++
    b		div_loop				; keep going

div_stop:
    mov		r1,		r0			; return the remainder as well
    mov		r0,		r4			;
    ldmfd	r13!,	{r4, pc}		;

div_test:
    stmdb  r13!,   {r0-r8, r10-r11, lr}
    mov		r0,		#0x100
    mov		r1,		#0x2
    bl		div
    ldmfd   r13!,   {r0-r8, r10-r11, pc}

;============================
;		swap
;		swap r0 and r1
;============================

swap:
    add		r0,		r0,		r1	; a = a + b
    sub		r1,		r0,		r1	; b = a - b
    sub		r0,		r0,		r1	; a = a - b
    mov		pc,		lr			;


;============================
;		memset
; 	set all memory at address r0,
; 	to value r1 (DWORD),
;	for r2 bytes
;============================

memset:
    stmdb	r13!,	{r3-r8, r10-r11, lr}
    mov		r11,	r13
    ;mov     r4,     r2,     lsr     #0x2        ; set iteration count = r2 >> 2 = r2 / 4
    mov		r5,		#0x0					; counter
    and     r1,     r1,     #0xFF           ; first byte only

    mov     r6,     r1,     lsl     #0x18           ; r6 = r1 << 24
    orr     r6,     r6,     r1,     lsl     #0x10   ; r6 |= (r1 << 16)
    orr     r6,     r6,     r1,     lsl     #0x8    ; r6 |= (r1 << 8)
    orr     r1,     r1,     r6                      ; r1 |= r6 (word is filled with the same byte)

    and		r6,		r2,		#0x3		; calc remainder: r6 = r2 % 4
    bic     r2,     r2,     #0x3        ; Ditch remainder: r2 = r2 & (~0x3)

memset_loop:
    cmp		r5,		r2					; r5 < r2 ? break : keep iterating
    bge		memset_loop_fin				; will be true if there is a remainder as well as if there isn't
    str		r1,		[r0, r5]		    ; *(r0 + i * 4) = r1
    add		r5,		r5,		#0x4		; increment by 4 bytes
    b		memset_loop

memset_loop_fin:
	cmp		r6,		#0x0				; check to see if we had a rem
	beq		memset_fin					; nope, so bail
    ldr     r7,     [r0, r5]            ; load all four
    mov     r8,     #0x4
    sub     r8,     r8,     r6          ; r8 = 4 - r6 = the amount in r7 to preserve
    mov     r4,     #0xFF000000         ; begin mask
    mov     r2,     #0x1                ; start
    mov     r3,     #0x8                ; create a growable bitmask
    mov     r6,     #0xFF000000

memset_mask_loop:                       ; grow the mask iteratively
    cmp     r2,     r8
    beq     memset_mask_loop_fin
    orr     r4,     r4,     r6,     lsr     r3
    add     r2,     r2,     #0x1
    add     r3,     r3,     #0x8
    b       memset_mask_loop

memset_mask_loop_fin:
    and     r3,     r7,     r4      ; store what we want to preserve in r3
    mov     r6,     r0              ; save the address
    bic     r6,     r1,     r4      ; r6 = r1 & (~r4)
    orr     r3,     r6,     r3      ; r3 |= r6
    str     r3,     [r0,    r5]     ; done

memset_fin:
    ldmfd	r13!,	{r3-r8, r10-r11, pc}

memset_test:
    stmdb	r13!,	{r0-r8, r10-r11, lr}	; setup our stack
    mov		r11,	r13
    sub		r13,	r13,	#0xC			; alloc n bytes

    mov		r0,		r13  					; pointer
    mov     r1,     #0xEF
    mov		r2,		#0x9
    bl		memset
    add     r13,    r13,    #0xC
    ldmfd  r13!,    {r0-r8, r10-r11, pc}

;============================
;		negate
;		returns -a
;============================
negate:
    stmdb	r13!,	{r3-r8, r10-r11, lr}
    mov		r4,		r0			; tmp = r0

    bl		wordmask			         ; r0 = 0xFFFFFFFF
    bic		r0,		r0,		r4	         ; r0 = r0 & ( ~tmp )
    add		r0,		r0,		#0x1         ; two's compliment

    ldmfd	r13!,	{r3-r8, r10-r11, pc}

;============================
;		abs
;		returns |a|
;============================

abs:
    stmdb	r13!,	{r11, lr}
    cmp		r0,		#0x0
    bge		abs_end			; a < 0?
    bl		negate			; a = -a
abs_end:
    ldmfd	r13!,	{r11, pc}

abs_test:
    stmdb  r13!,   {r0-r8, r10-r11, lr}
    mov		r0,		#-4
    bl		abs
    orr		r0,		r0,		#0x0
    ldmfd  r13!,   {r0-r8, r10-r11, pc}
;end

;============================
;		smul
;		returns a * b, where
;		a -> r0
;		b -> r1
;		is relatively fast given that (a < 10 || b < 10)
;
;		registers used:
;
;		r0, r1 (params)
;		r4 - r6 (locals)
;		lr, pc
;============================

smul:
    stmdb	       r13!,	          {r4-r6, r11, lr}
    mov		       r6,		      #0x0			; = sign_flags

    cmp		       r0,		      #0x0			; a < 0 ?
    blt		       smul_negate0			; make a note of it...
    b		       smul_cmpr1
smul_negate0:
    orr		       r6,	    r6,	   #0x1	; sign_flags |= 1
    bl		       negate				; = abs(a)

smul_cmpr1:
    cmp		       r1,			   #0x0	; b < 0 ?
    blt		       smul_negate1			; make a note of it...
    b		       smul_check_swap
smul_negate1:
    orr		       r6,		r6,		#0x2	  ; sign_flags |= 2
    mov		       r4,		r0			      ; shuffle things around so we can pass r0 = r1 to negate function
    stmdb	       r13!,	{r4}			; save r4, since it's used by negate

    mov		       r0,		r1
    bl		       negate

    ldmfd	       r13!,    {r4}

    mov		       r1, 		r0
    mov		       r0,		r4

smul_check_swap:
    cmp		       r0,		r1		; swap a and b if a < b, to decrease iteration count
    blt		       smul_swap
    b		       smul_loop
smul_swap:
    bl		       swap

    mov		       r4,		#0x1		; = count
    mov		       r5,		r0		; = a

smul_loop:									; do the actual multiplication...
    cmp		       r4,		r1
    bge		       smul_has_negative
    add		       r0,		r0,		r5
    add		       r4,		r4,		#0x1
    b		       smul_loop

smul_has_negative:							; (a < 0) or (b < 0)?
    cmp		r6,		#0x0
    bne		smul_check_negative	; if so, do we have two negative values or one?
    b		smul_return

;		compute the value of (r6 & 0x1) ^ ((r6 & 0x2) >> 1).
;		if the value is > 0, we have a negative
smul_check_negative:
    and		r4,		r6,		#0x2
    ;lsr		r4,		r4,		#0x1
    mov     r4,     r4,     lsr     #0x1
    and		r5,		r6,		#0x1
    eor		r5,		r4,		r5
    cmp		r5,		#0x0
    bgt		smul_negate_return
    b		smul_return
smul_negate_return:
    bl		negate

smul_return:
    ldmfd	r13!,	{r4-r6, r11, pc}

smul_test:
    stmdb  r13!,   {r0-r8, r10-r11, lr}
    mov		r0,		#0x3
    mov		r1, 	#-4
    bl		smul
    ldmfd  r13!,    {r0-r8, r10-r11, pc}

lmul:
    stmdb	r13!,   {r0-r8, r10-r11, lr}
    mov		r11,		r13
    sub		r13,		r13,		#0x34 	; a, the remainder, x (the absolute value of b),
            ; the sign, x's iterator, and (up to) 9 sequential 32-bit sums
            ; the sign and x's iterator can both fit into 16 bits

    mov

    ;------------------------------
    ; assess the signs of a and b
    ;------------------------------

    mov		r4,		#0x0

    cmp		r0,		#0x0  			; a < 0 ? then sign |= 0x1
    bge		lmul_check_b_sign
    orr		r4,		r4,		#0x1

lmul_check_b_sign:
    cmp		r1,		#0x0				; b < 0 ? then sign |= 0x2
    bge		lmul_store_sign
    orr		r4,		r4,		#0x2

lmul_store_sign:
    str		r4,		[r11, 	#-12]	; store the sign value for later

;------------------------------
; initialize the other locals
;------------------------------

    ; store a's abs val, which is used as an initialization value in the loop
    bl		abs
    str		r0,		[r11,	#-20]


    ; now, store b's abs val
    mov		r0,		r1
    bl		abs
    str		r0,		[r11, 	#-4]		; x = abs(b)

    ; take care of the rest
    mov		r4,		#0x0
    str		r4,		[r11,	#-8] 	; remainder = 0
    str		r4,		[r11,	#-16]	; xj = 0 (x's iterator)

    ; MEMSET the buffer of 9 * 4 bytes of memory - remember, these are integers
    ; representing the sums
    mov     r0,     r13             ; base is bottom


    ldmfd	r13!,	{r0-r8, r10-r11, lr}


is_mul_of_five:
    stmdb	r13!,	{r11, lr}

;		store our fixed point constant for division by five: we begin with the following principal (2^16 / 5) ~=~ 0x3333.
;		if we shift left by 8 bits, we have: (2^24 / 5) ~=~ 0x333333
;		we add 8 bits of decimal precision, which provides us with a value of 0x333300 + 0x33
;		from here, as long as we shift our number to multiply this by 8 as well, we can extract
;		the first 8 bits and use those as a valid mechanism to determine whether or not the quotient
;		is an even division.


;		you need to:
;		take whatever is in r0 and shift it left by 8
;		multiply it by the constant
;		and the result by 0x0000FF
;		if the result is != 0, then no this isn't a multiple of five

    mov		r1,		#0x3300
    add		r1,		r1,		#0x0033

    bl		smul
    mov		r0,		r0,	      lsr	#0x8			; shift left by 8 to make room for remainder

    and		r0,		r0,		#0x0000FF		;
    cmp		r0,		#0x0
    bne		imof_false
    mov		r0,		#0x1					; return true
    b		imof_end

imof_false:
    mov		r0,		#0x0					; return false
imof_end:
    ldmfd	r13!,	{r11, pc}





_start:
    bl      memset_test
    b       exit


    stmdb	r13!, 	{r4, r11, lr}

    mov		r11,		r13
    sub		r13,		r13,		#0xC

    mov		r0, 		#0x0

    ;		store our five multiplier
    str		r0, 		[r11, 	#-4]

    ;		store our three multiplier
    str		r0,		[r11, 	#-8]

    ;		store our three multiplier termination value
    mov		r0,		#0x03E0
    add		r0,		r0,		#0x0004
    str		r0,		[r11, 	#-12]

    ;		begin
loop:
    ldr		r0,		[r11, 	#-4]
    add		r3,		r0,		#0x5
    cmp		r3,		#0x03E8			; five multiplier termination - doesn't need to be stored

    bne		add_five
    b		check_three

add_five:
    add		r2,		r2,		r3
    str		r3,		[r11, 	#-4]

check_three:
    ldr		r1,		[r11,	#-8]
    add		r3,		r1,		#0x3

    mov		r4,		#0x03E0
    add		r4,		r4,		#0x000A

    cmp		r3,		r4

    bne		add_three
    b		stop

add_three:
    ;		check to see if the value is also a multiple of five: if it is, we skip.
    mov		r0,		r3
    bl		is_mul_of_five
    cmp		r0,		#0x1
    bne		continue_add_three
    b		loop

continue_add_three:

    add		r2,		r2,		r3
    str		r3,		[r11, 	#-8]

    b		loop

    add		r13,		r13,		#0xC
stop:
    mov		r0,		r0

exit:
	mov    r0, #0
	swi    0x11
