; Public domain

	device ZXSPECTRUM48

	org #9000

GRID_WIDTH		EQU 85
GRID_HEIGHT		EQU 64
GRID_SIZE		EQU GRID_WIDTH * GRID_HEIGHT

DIR_Z			EQU 128
MAX_DIST		EQU 64 * 128
PLANE_Y			EQU (-2 * 128)

DIR_LIGHT_X		EQU ((-577 * 128) / 1000)
DIR_LIGHT_Y		EQU ((577 * 128) / 1000)
DIR_LIGHT_Z		EQU ((-577 * 128) / 1000)

DIR_LIGHT_A		EQU 246

MAX_BRIGHTNESS		EQU 16

savebin_begin:
	di	
	ld bc, #17ff
	ld hl, #4000
	ld (hl), l
	ld d, h
	ld e, l
	inc de
	ldir

	ld bc, #2ff
	ld hl, #5800
	ld (hl), l
	ld d, h
	ld e, l
	inc de
	ldir

	xor a
	out (#fe), a

	call CRT

	call raytrace
	jr $

	ret

; -----------------------------------------------------------------------------------------------------------------------
; Signed Division 16/16 fp 9.7
; hl = hl / bc
; fp I.F means we're dividing x*2^F by y*2^F and want to get z*2^F.
; In order to not lose precision, premultiply x by 2^F then and divide using 32-bits
sdiv16:
	bit 7, h
	jr z, sdiv16_dividend_positive
	bit 7, b
	jr z, sdiv16_dividend_negative_divisor_positive
	; both negative
	; just invert them and divide as unsigned
	ld de, 0
	ex de, hl
	or a
	sbc hl, de
	push hl
	ld hl, 0
	or a
	sbc hl, bc
	ld b, h
	ld c, l
	pop hl
	jp udiv16

sdiv16_dividend_negative_divisor_positive:
	; dividend negative, divisor positive
	; invert the dividend, divide unsigned, and invert back
	ld de, 0
	ex de, hl
	or a
	sbc hl, de
	call udiv16
	ld de, 0
	ex de, hl
	or a
	sbc hl, de
	ret

sdiv16_dividend_positive:
	bit 7, b
	jr z, udiv16	; both positive, proceed straight to division
	; dividend positive, divisor negative
	; invert the divisor, divide unsigned, and invert back
	ld a, b
	cpl
	ld b, a
	ld a, c
	cpl
	ld c, a
	inc bc
	call udiv16
	ld de, 0
	ex de, hl
	or a
	sbc hl, de
	ret

; -----------------------------------------------------------------------------------------------------------------------
; Unsigned 16/16 fp 9.7 division
; hl = hl / bc
; fp I.F means we're dividing x*2^F by y*2^F and want to get z*2^F.
; In order to not lose precision, premultiply x by 2^F then and divide using 32-bits
udiv16:
	; restoring algorithm of 24/16 division, hl is the 16-bit accumulator, de:a holds the 24-bit dividend (will also become quotient)
	; we make that 24 bit number out of 16 bit number shifted to the left
	; so shift the numerator 7 bits to the left
	ex de, hl
	xor a
	ld h, a		; accumulator HL starts at 0
	ld l, a

	; We need to shift a 9.7fp number by 7, but we instead shift by 8 and use 23 iterations. We only need 23 bits anyway (9.7 fp << 7)
	;sra d
	;rr e
	;rra

	dup 23 		
	  sla a
	  rl e
	  rl d
	  rl l
	  rl h
	  or a
	  sbc hl, bc
          inc a
	  bit 7, h  
	  jr z, 1F
	  dec a
	  add hl, bc
1
	edup

	ld h, e
	ld l, a
	ret

; -----------------------------------------------------------------------------------------------------------------------
; Signed multiply 16/16 fp 9.7
; hl = de * bc
; fp I.F means we're multiplying x*2^F by y*2^F and want to get z*2^F
; after multiply, we would get xy*2^(F+F), so divide the result by 2^F (shift by 7 bits)
smul16:
	bit 7, d
	jr z, smul16_de_positive
	bit 7, b
	jr z, smul16_de_negative_bc_positive
	; // both negative
	; // just invert them and multiply as unsigned
	ld hl, 0
	or a
	sbc hl, de
	push hl
	ld hl, 0
	or a
	sbc hl, bc
	ld b, h
	ld c, l
	pop de
	jp umul16

smul16_de_negative_bc_positive:
	; de negative, bc positive
	; invert de, multiply unsigned, and invert back
	ld hl, 0
	or a
	sbc hl, de
	ex de, hl
	call umul16
	ld de, 0
	ex de, hl
	or a
	sbc hl, de
	ret

smul16_de_positive:
	bit 7, b
	jr z, umul16	; both positive, proceed straight to multiplication
	; de positive, bc negative
	; invert bc, multiply unsigned, and invert back
	ld hl, 0
	or a
	sbc hl, bc
	ld b, h
	ld c, l
	call umul16
	ld de, 0
	ex de, hl
	or a
	sbc hl, de
	ret

; -----------------------------------------------------------------------------------------------------------------------
; 16-bit 9.7 fixed point unsigned multiply
;   hl = bc * de
; credits: https://tutorials.eeems.ca/Z80ASM/part4.htm
; fp I.F means we're multiplying x*2^F by y*2^F and want to get z*2^F
; after multiply, we would get xy*2^(F+F), so divide the result by 2^F (shift by 7 bits)
umul16:
	ld hl, 0
	ld a, b
	and a
	jr nz, umul16_bc_larger_than_256
	or c
	ret z	; multiplication by 0
	; we know that bc is non-zero and 8 bit
	ld a, d
	and a
	jr nz, umul16_regular_multiply	; bc 8 bit, but de 16 -> no special case
	; at this point we know that both a 8 bit
	or e
	ret z   ; multiplication by 0
	;jr umul16_regular_multiply

umul8:
	; do a 8-bit multiply
	ld h, c
	dup 8
	  add hl,hl
	  jr nc, 1F
	  add hl, de
1
	edup
	; we know that hl holds 2.14 fixed point value. We need to make a 9.7 out of it. Shift o times left and take 
	ld e, 0
	sla l
	rl h
	rl e
	; don't care about d
	ld l, h
	ld h, e
	ret

umul16_bc_larger_than_256:
	; bc is larger than 256, cannot use umul8, but still want to check if de is 0
	ld a, d
	or e
	ret z
	; intentional fall-through to regular multiply

umul16_regular_multiply:
	dup 16
	  add hl, hl
	  rl e
	  rl d
	  jr nc, 1F
	  add hl, bc
	  jr nc, 1F
	  inc de                         ; This instruction (with the jump) is like an "ADC DE,0"
1
	edup
	; we have de:hl holding a 18.14 result, and we need to take the middle 9.7 part. 
	; Shift everything one bit left and then take e, h
	sla l
	rl h
	rl e
	; don't care about d
	ld l, h
	ld h, e
	ret

; -----------------------------------------------------------------------------------------------------------------------
; Generates a random 16-bit number
; Returns in hl
; Has no parameters, isolated
Rnd16:
	; Original description:
	;Tested and passes all CAcert tests
	;Uses a very simple 32-bit LCG and 32-bit LFSR
	;it has a period of 18,446,744,069,414,584,320
	;roughly 18.4 quintillion.
	;LFSR taps: 0,2,6,7  = 11000101
	;291cc
seed1_0 = $ + 1
	ld hl, 12345
seed1_1 = $ + 1
	ld de, 6789
	ld b, h
	ld c, l
	add hl, hl
	rl e
	rl d
	add hl, hl
	rl e
	rl d
	inc l
	add hl, bc
	ld (seed1_0), hl
	ld hl, (seed1_1)
	adc hl, de
	ld (seed1_1), hl
	ex de, hl
seed2_0=$+1
	ld hl, 9876
seed2_1=$+1
	ld bc, 54321
	add hl, hl
	rl c
	rl b
	ld (seed2_1), bc
	sbc a, a
	and %11000101
	xor l
	ld l, a
	ld (seed2_0), hl
	ex de,hl
	add hl, bc
	ret

; -----------------------------------------------------------------------------------------------------------------------
; Called whenever a single ray is calculated
; a contains the last calculated value
OnRayCalculated:
	and a
	ret z

	ld (NumDotsToPlot), a

NumDotsToPlot equ $+1
	ld b, 1

        macro PLOT_LOOP_BODY
	  ; we know b != 0, so we can move the check after 1 iteration
	; take next 4 random bits
	  ld a, l
	  and #0F

	  srl h
	  rr l
	  srl h
	  rr l
	  srl h
 	  rr l
	  srl h
	  rr l

	  ld de, (ScreenY)	; ScreenX will be in D

	  ; convert [-3, 4] range into [-1,2] range, kind-of approximating the gaussian distribution (because from -3, -2, -1, 0, 1, 2, 3, 4 it will become -2, -1, -1, 0, 0, 1, 1, 2) 
	  ld c, a
	  and #07
  	  sub 3
	  sra a
	  add d
	  ld d, a

	  ld a, c
	  rrca
	  and #07
	  sub 3
	  sra a
	  add e
	  cp 192	; if beyond the screen, skip
	  jr nc, 1F
	  ld e, a

	  push bc
	  push hl
	  call ZIP
	  pop hl
	  pop bc
1
	  dec b
	  jp z, DonePlotting
	endm

	; generate a quality 16-bit random from https://wikiti.brandonw.net/index.php?title=Z80_Routines:Math:Random#Combined_LFSR.2FLCG.2C_32-bit_seeds
	push bc
	call Rnd16
	pop bc
	
	dup 4
	  PLOT_LOOP_BODY
	edup

	push bc
	call Rnd16
	pop bc

	dup 4
	  PLOT_LOOP_BODY
	edup

	push bc
	call Rnd16
	pop bc

	dup 4
	  PLOT_LOOP_BODY
	edup	

	push bc
	call Rnd16
	pop bc

	dup 4
	  PLOT_LOOP_BODY
	edup	

DonePlotting:

	; now paint the attributes
	ld bc, (ScreenY)
	srl b
	srl b
	srl b
	;sra c
	;sra c
	;sra c

	; credits: https://espamatica.com/zx-spectrum-screen/#attribute-address-from-character-coordinates
	ld a, c
	rlca
	rlca
	;rrca
	ld l, a
	and #03
	or #58
	ld h, a
	ld a, l
	and #e0
	or b
	ld l, a	

	; see if we need to set bright
	ld a, (NumDotsToPlot)
	cp 8
	jr c, NoBright

	ld a, #40
	or (hl)
	ld (hl), a

NoBright:
	; compare if we need to up the ink to match current brightness
	ld a, (NumDotsToPlot)
	ld e, a
	ld d, high InkColorTab
	ld a, (de)
	ld b, a

	ld a,(hl)
	and #07

	cp b	; ink < TabInk?  
	ret nc	; no color change needed, we already have it >= than what we want for this brightness

	; otherwise, replace with the table value
	ld a, #F8
	and (hl)
	or b
	ld (hl), a
	ret


; -----------------------------------------------------------------------------------------------------------------------
; Traces a single ray
; Expects variables: Spheres, DirX, DirY, DirX_Squared, DirY_Squared
; Returns A reg as the light intensity value
TraceRay:
	ld hl, (DirX_Squared)
	ld bc, (DirY_Squared)
	add hl, bc
	ld bc, DIR_Z	; dir z being 1 is the same squared
	add hl, bc
	or a
	rl l
	rl h
	ld (TraceRay_A), hl

	ld hl, MAX_DIST
	ld (Dist), hl

	xor a
	ld (SphereHit), a
	ld (SphereIndex), a

	ld ix, Spheres_table
	dup 3	; // we know we have three spheres

	  ; Calculate B = (smul16(DirX, Sphere[0]) + smul16(DirY, Sphere[1]) + Sphere[2]/*smul16(DirZ, Sphere[2])*/) << 1;
	  ld bc, (DirX)
	  ld e, (ix + 0)
	  ld d, (ix + 1)
	  inc ix
	  inc ix
	  call smul16
	  push hl
	  ld bc, (DirY)
	  ld e, (ix + 0)
	  ld d, (ix + 1)
	  inc ix
	  inc ix
	  call smul16
	  ld e, (ix + 0)
	  ld d, (ix + 1)
	  inc ix
	  inc ix
	  inc ix
	  inc ix
          add hl, de
	  pop de
          add hl, de
	  or a
          rl l
          rl h  
          ld (TraceRay_B), hl

	  ; C is just Sphere[4], which is sphere + 8, sphere + 9

	  ; calculate B^2
	  ex de, hl
	  ld b, d
	  ld c, e
	  call smul16
	  push hl	

          ; calculate 2*A*C
	  ld bc, (TraceRay_A)
          ld e, (ix + 0)
	  ld d, (ix + 1)
          inc ix
	  inc ix				; ix is now pointing to the next sphere
	  call smul16
	  or a
          rl l
          rl h  

	  ; calculate D = B^2 - 2*A*C
	  ex de, hl
	  pop hl
	  or a
	  sbc hl, de

	  ; D < 0?  Then move on to next sphere
	  bit 7, h
          jr nz, 1F	; next sphere

	  ; calculate B-D
	  ex de, hl
	  ld hl, (TraceRay_B)
	  or a
	  sbc hl, de

	  ; calculate T = sdiv16((B - D), A);
	  ld bc, (TraceRay_A)
	  call sdiv16

	  ; T < 0? Again, next sphere
          bit 7, h
	  jr nz, 1F

	  ; if T < Dist, this is our sphere
	  ld bc, (Dist)
	  or a
	  sbc hl, bc
	  add hl, bc
	  ; nc if hl >= bc, so T >= Dist, so not a hit
	  jr nc, 1F

	  ; hit a sphere!
	  ld (Dist), hl
          ld a, (SphereIndex)
	  inc a
	  ld (SphereHit), a
          jp TraceRay_CheckPlaneIntersection
1
	  ld hl, SphereIndex
	  inc (hl)
	edup

TraceRay_CheckPlaneIntersection:
	ld bc, (DirY)
	bit 7, b
	jr z, CheckMaxDist
	ld hl, PLANE_Y
	call sdiv16

	; T < 0? No hit
        bit 7, h
	jr nz, CheckMaxDist

	; if T < Dist, we have a hit
	ld bc, (Dist)
	or a
	sbc hl, bc
	add hl, bc
	jr nc, CheckMaxDist

	; count as a hit on plane, and save plane normal
	ld (Dist), hl
	xor a
	ld (SphereHit), a

Normal_Perturb equ $+1
	ld a, 0
	add 13
	and #0f
	ld (Normal_Perturb), a

	ld hl, 0
	ld (NZ), hl
	ld l, a
	ld (NX), hl
	ld hl, 128
	ld (NY), hl

CheckMaxDist:
	ld hl, MAX_DIST
	ld bc, (Dist)
	or a
	sbc hl, bc
	jr nz, NoSkyHit

	; hit the sky. Mutate sky color (the generated picture is static anyway) to make it more interesting
Sky_color equ $+1
	ld a, 1
	dec a
	and 1
	ld (Sky_color), a
	ret

NoSkyHit:
	; Calculate the hit point
	; bc still holds dist
	ld de, (DirX)
	call smul16
	ld (PtX), hl

	ld de, (DirY)
	ld bc, (Dist)
	call smul16
	ld (PtY), hl

	; PtZ is always Dist

	ld a, (SphereHit)
	and a
	jr z, CalcLighting		;	if all we hit is a plane, we're ready to calculate the lighting

	; otherwise, calculate the normal
	; need to multiply SphereHit (remember it's Index + 1) by 10, that is, 8+2
	ld ix, Spheres_table
	dec a
	; small opt - remove if size matters
	jr z, TraceRay_SphereNormalCalc_NoOffsetNeeded
	rlca	; x2
	ld c, a
	rlca	; x4
	rlca	; x8
	add c
	ld c, a
	ld b, 0
	add ix, bc

TraceRay_SphereNormalCalc_NoOffsetNeeded:
	; using the fact that our spheres have R = 1, so the vector from the center of the sphere to a point on it is also the normal to that point
	ld hl, (PtX)
	ld e, (ix + 0)
	ld d, (ix + 1)
	inc ix
	inc ix
	or a
	sbc hl, de
	ld (NX), hl

	ld hl, (PtY)
	ld e, (ix + 0)
	ld d, (ix + 1)
	inc ix
	inc ix
	or a
	sbc hl, de
	ld (NY), hl

	ld hl, (PtZ)
	ld e, (ix + 0)
	ld d, (ix + 1)
	inc ix
	inc ix
	or a
	sbc hl, de
	ld (NZ), hl

	; intentional fall-through

; -----------------------------------------------------------------------------------------------------------------------
; Calculates lighting
; Expects variables: Spheres, DirX, DirY, DirX_Squared, DirY_Squared, PtX, PtY, PtZ, NX, NY, NZ
; Returns register A as the light intensity value
CalcLighting:

	; trace against the spheres to determine if we're in the shadow

	; sphere hit variable is used to avoid tracing with the sphere that we hit with the primary ray (relying on the NdotL to shadow us if we're on the backfacing side)
	xor a
	ld (SphereIndex), a
	ld (InShadow), a

	ld ix, Spheres_table
	dup 3	; // we know we have three spheres
	  ld a, (SphereIndex)
	  inc a ; sphere hit is incremented by one
	  ld b, a
	  ld a, (SphereHit)
	  cp b
	  jp z, 1F 	; // skip tracing with this sphere

	  ; first we need to calculate hit point relative to this sphere
	  ld hl, (PtX)
	  ld e, (ix + 0)
	  ld d, (ix + 1)
	  inc ix
	  inc ix
	  or a
	  sbc hl, de
	  ld (Rel_PtX), hl

	  ld hl, (PtY)
	  ld e, (ix + 0)
	  ld d, (ix + 1)
	  inc ix
	  inc ix
	  or a
	  sbc hl, de
	  ld (Rel_PtY), hl

	  ld hl, (PtZ)
	  ld e, (ix + 0)
	  ld d, (ix + 1)
	  inc ix
	  inc ix
	  or a
	  sbc hl, de
	  ld (Rel_PtZ), hl

	  ; Calculate B = -((smul16(Rel_PtX, DirLightX) + smul16(Rel_PtX, DirLightY) + smul16(Rel_PtX, DirLightZ)) << 1);
	  ; hl still holds Rel_PtZ
	  ex de, hl
	  ld bc, DIR_LIGHT_Z
	  call smul16
	  push hl

	  ld de, (Rel_PtX)
	  ld bc, DIR_LIGHT_X
	  call smul16
	  push hl

	  ld de, (Rel_PtY)
	  ld bc, DIR_LIGHT_Y
	  call smul16

	  pop de
	  add hl, de

	  pop de
	  add hl, de

	  sla l
	  rl h

	  ex de, hl
	  ld hl, 0
	  or a
	  sbc hl, de
          ; B is ready
	  ld (TraceRay_B), hl

	  ; calculate C = smul16(PtX, PtX) + smul16(PtY, PtY) + smul16(PtZ, PtZ) - Sphere[3];
	  ld de, (Rel_PtX)
	  ld b, d
	  ld c, e
	  call smul16
	  push hl

	  ld de, (Rel_PtY)
	  ld b, d
	  ld c, e
	  call smul16
	  push hl

	  ld de, (Rel_PtZ)
	  ld b, d
	  ld c, e
	  call smul16

	  pop de
	  add hl, de

	  pop de
	  add hl, de

	  ; ix is already at Sphere3
	  ld e, (ix + 0)
	  ld d, (ix + 1)

	  or a
	  sbc hl, de
	  ; C is ready
	  ld (TraceRay_C), hl

	  ; calculate D = smul16(B, B) - (smul16(Light_A, C) << 1);
	  ; hl already holds C
	  ex de, hl
	  ld bc, DIR_LIGHT_A
	  call smul16
	  sla l
	  rl h
	  push hl 

	  ld de, (TraceRay_B)
	  ld b, d
	  ld c, e
	  call smul16

	  pop de
	  or a
	  sbc hl, de

	  ; D is ready
	  bit 7, h
	  jr nz, 1F	;	// sphere was not hit

	  ; calculate T = sdiv16((B + D), Light_A);
	  ; hl already holds d
	  ld de, (TraceRay_B)
	  add hl, de
	  ex de, hl

	  ld bc, DIR_LIGHT_A
	  call sdiv16

	  ; if (T > 0), we hit a sphere
	  bit 7, h
	  jr nz, 1F

	  ; hit a sphere!
	  ld a, 1
	  ld (InShadow), a
          jp CalcLighting_CalculateRegularTerm
1
	  ld hl, SphereIndex
	  inc (hl)
	  ; ix needs to be incrementd by 4 more bytes to point to the next sphere
	  inc ix
	  inc ix
	  inc ix
	  inc ix
	edup

CalcLighting_CalculateRegularTerm:
	; regular lighting, NdotL = smul16(NX, DirLightX) + smul16(NY, DirLightY) + smul16(NZ, DirLightZ);
	; normal is expected to be normalized (it better be!  we'll ignore the distortions if it isn't - and we do perturb it)
	ld de, (NX)
	ld bc, DIR_LIGHT_X
	call smul16
	push hl

	ld de, (NY)
	ld bc, DIR_LIGHT_Y
	call smul16
	push hl

	ld de, (NZ)
	ld bc, DIR_LIGHT_Z
	call smul16
	pop de
	add hl, de
	pop de
	add hl, de

	xor a
	bit 7, h
	ret nz		; no light, NdotL is < 0

	ld b, h                                                                                                                                                                                   	
	ld c, l

	; calculate ZFade =  max(0, MAX_BRIGHTNESS - Z)
	ld hl, MAX_BRIGHTNESS * 128	; MAX_BRIGHTNESS in 9.7 fixedpoint
	ld de, (PtZ)
	xor a
	sbc hl, de
	; in case we have Z > 16, return 0
	ret c

	; here we'll check the shadow
	ld a, (InShadow)
	and a
	jr z, CalcLighting_NotInShadow

	sra h
	rr l
	sra h
	rr l

CalcLighting_NotInShadow:	
	; multiply ZFade by NdotL
	ex de, hl
	call smul16

	; and take its integer value
	ld a, h
	sla l
	rla
	
	; but everything above MAX_BRIGHTNESS is out
	cp MAX_BRIGHTNESS + 1
	ret c

	ld a, MAX_BRIGHTNESS
	ret

; -----------------------------------------------------------------------------------------------------------------------
; Raytraces screen with a GRID_WIDTH x GRID_HEIGHT grid where each ray has MAX_BRIGHTNESS intensity value.
; Expects variables: Spheres
; Sets variables: DirX, DirY, DirX_Squared, DirY_Squared
raytrace:
	ld b, GRID_HEIGHT

raytrace_y_loop:
	ld a, b
	dec a
	ld c, a
	rlca
	add c
	inc a
	ld (ScreenY), a		; ScreenY = (64 - YCounter) * 3

	push bc

	; calculate RayDirY = (short)sdiv16((Y - 32) * FIXEDPOINT_SCALER, 64 * FIXEDPOINT_SCALER);
	; except our Y = 64 - YCounter, so the above becomes
	; RayDirY = (short)sdiv16((32 - YCounter) * FIXEDPOINT_SCALER, 64 * FIXEDPOINT_SCALER)
	; we're essentially going to divide the number by 64, which is shift right to 6 bits. Note that we FIXEDPOINT_SCALER is 7 bits, so the end result is a 1 bit left shift
	ld a, GRID_HEIGHT / 2
	sub b
	ld l, a
	sbc a, a	; // sign-extend
	ld h, a
	sla l		; shouldn't need to touch h as we don't come close to 7 bit numbers here
	ld (DirY), hl

	ex de, hl
	ld b, d
	ld c, e
	call smul16
	ld (DirY_Squared), hl

	ld b, GRID_WIDTH

raytrace_x_loop:
	ld a, GRID_WIDTH
	sub b
	ld c, a
	rlca
	add c
	inc a
	ld (ScreenX), a

	push bc

	; we can approximate RayDirX calculation as RayDirX = (short)sdiv16(1.5 * (X - 42) * FIXEDPOINT_SCALER, 128 * FIXEDPOINT_SCALER);
	; given that our X = 85 - XCounter, we have
	; RayDirX = (short)sdiv16( ((43 - XCounter) + ((43 - XCounter)>>1)) * FIXEDPOINT_SCALER, 128 * FIXEDPOINT_SCALER);
	; FIXEDPOINT_SCALER is also 128, so we don't need to do anything

	ld a, GRID_WIDTH / 2
	sub b
	ld b, a
	sra a
	add b
	ld l, a
	rlca	; sign-extend
	sbc a, a
	ld h, a
	ld (DirX), hl

	ex de, hl
	ld b, d
	ld c, e
	call smul16
	ld (DirX_Squared), hl

	call TraceRay
	; put the ray on the screen!
	call OnRayCalculated

	pop bc
	djnz raytrace_x_loop

	pop bc
	djnz raytrace_y_loop
	ret

; "ZIP PLOT 1.4"
; Credits: Viper of TechnoLab
; Taken from KrNews #05 (July 24, 1998)
; https://zxpress.ru/article.php?id=8242
ZIP    LD L,E
       LD H,high PTY
       LD B,(HL)
       DEC H
       LD A,(HL)
       DEC H
       LD L,D
       ADD A,(HL)
       LD C,A
       DEC H
       LD A,(BC)
       OR (HL)
       LD (BC),A
       RET 

CRT    LD HL,PTY   ;   table location
       LD DE,#4000 ;   main scren
       LD BC,#C020
CR1    LD (HL),D
       DEC H
       LD (HL),E
       INC L
       INC H
       INC D
       LD A,D
       AND 7
       JR NZ,$+12
       LD A,E
       SUB #E0
       LD E,A
       JR NC,$+6
       LD A,D
       SUB 8
       LD D,A
       DJNZ CR1
       LD L, #FF
       DEC H  
       DEC H
CR3    DEC C
       LD B,8
CR2    LD (HL),C
       DEC L
       DJNZ CR2
       INC C
       DEC C
       JR NZ,CR3
       DEC H
       INC A
CR4    LD (HL),A
       RLCA 
       DEC L
       DJNZ CR4
       RET

; ---------------------
; Sphere data (thoroughly precalculated in the prototypator)
Spheres_table:
	dw -176, 0, 384, 128, 1266
	dw 48, 128, 640, 128, 3218
	dw 400, 0, 1152, 128, 11490

	align 256
InkColorTab:	; aligned by 256, indexed by brightness
	db 5, 5, 5, 5,   5, 7, 7, 7,   7, 7, 7, 7,   7, 7, 7, 7,   7, 7, 7, 7
	assert MAX_BRIGHTNESS <= 20, Increase InkColorTab if brightness is higher than 16

	savebin "rtzx_main.bin", savebin_begin, $-savebin_begin

; ---------------------
; raytrace variables (will be set before being used, so safe to leave outside the saved area)
ScreenY:
	db 0
ScreenX:	; must be immediately following ScreenY
	db 0

; ray X direction
DirX:
	dw 0

; ray X direction squared (precalc for speed)
DirX_Squared:
	dw 0

; ray Y direction
DirY:
	dw 0

; ray Y direction squared (precalc for speed)
DirY_Squared:
	dw 0

; TraceRay vars
; first term
TraceRay_A:
	dw 0
TraceRay_B:
	dw 0
TraceRay_C:
	dw 0

; current dist
Dist:
	dw 0
SphereHit:
	db 0
SphereIndex:
	db 0
InShadow:
	db 0

; point that we hit
PtX:
	dw 0
PtY:
	dw 0
PtZ equ Dist		; since we trace with DirZ == 1, PtZ = Dist * DirZ is equal to Dist
;	dw 0

; normal at the hit
NX:
	dw 0
NY:
	dw 0
NZ:
	dw 0

; hit point relative to a sphere (used in ligthing calculations)
Rel_PtX:
	dw 0
Rel_PtY:
	dw 0
Rel_PtZ:
	dw 0

	; helper tables for the plot routine
	align 256
	block 1024
PTY 	EQU $

