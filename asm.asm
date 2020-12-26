; *******************************************************************
; *** This software is copyright 2004 by Michael H Riley          ***
; *** You have permission to use, modify, copy, and distribute    ***
; *** this software so long as this copyright notice is retained. ***
; *** This software may not be used in commercial applications    ***
; *** without express written permission from the author.         ***
; *******************************************************************

include bios.inc
include kernel.inc

org:       equ     6000h

           org     8000h
           lbr     0ff00h
           db      'asm',0
           dw      9000h
           dw      endrom+9000h-org
           dw      org  
           dw      endrom-org  
           dw      org  
           db      0

           org     6000h
           br      start               ; jump over version

include    date.inc
include    build.inc
           db      'Written by Michael H. Riley',0

start:
           lda     ra                  ; move past any spaces
           smi     ' '
           lbz     start
           dec     ra                  ; move back to non-space character
           ldn     ra                  ; get byte
           lbnz    good                ; jump if nonzero
           sep     scall               ; otherwise display usage
           dw      f_inmsg
           db      'Usage: asm filename',10,13,0
           sep     sret                ; and return to os
good:      ldi     high symtab         ; set 0 pointer into symbol table
           phi     rf
           ldi     low symtab
           plo     rf
           ldi     0
           str     rf
           ldi     high lastsym        ; set last symbol address
           phi     rf
           ldi     low lastsym
           plo     rf
           ldi     high symtab
           str     rf
           inc     rf
           ldi     low symtab
           str     rf
           ldi     high pass           ; set pass to 0
           phi     rf
           ldi     low pass
           plo     rf
           ldi     0
           str     rf
           ldi     high linenum        ; set line number
           phi     rf
           ldi     low linenum
           plo     rf
           ldi     0
           str     rf
           inc     rf
           str     rf
           ldi     high fname          ; point to filename storage area
           phi     rf
           ldi     low fname
           plo     rf
           ldi     high fname2         ; point to filename storage area 2
           phi     r9
           ldi     low fname2
           plo     r9
fnamelp:   lda     ra                  ; get byte from filename
           str     rf                  ; store int buffer
           inc     rf
           str     r9
           inc     r9
           smi     33                  ; look for space or less
           lbdf    fnamelp             ; loop back until done
           dec     r9                  ; point back to termination byte
           dec     rf
           ldi     '.'                 ; append .asm
           str     rf
           inc     rf
           ldi     'a'                 ; append .asm
           str     rf
           inc     rf
           ldi     's'                 ; append .asm
           str     rf
           inc     rf
           ldi     'm'                 ; append .asm
           str     rf
           inc     rf
           ldi     0                   ; and write terminator
           str     r9
           str     rf
           ldi     high fname          ; point to filename storage
           phi     rf
           ldi     low fname
           plo     rf
           ldi     high fildes         ; point to file descriptor
           phi     rd
           ldi     low fildes
           plo     rd
           ldi     0                   ; flags
           plo     r7
           sep     scall               ; attempt to open the file
           dw      o_open
           lbnf    opened              ; jump if file was opened
openerr:   ldi     high fileerr        ; get file error message
           phi     rf
           ldi     low fileerr
           plo     rf
           sep     scall               ; display message
           dw      o_msg
           sep     sret                ; and return to caller
opened:    sep     scall               ; process the file
           dw      asmloop
           ldi     0                   ; seek file back to start
           phi     r7
           plo     r7
           phi     r8
           plo     r8
           plo     rc
           sep     scall
           dw      o_seek
           ldi     high pass           ; need to set pass to 1
           phi     rf
           ldi     low pass
           plo     rf
           ldi     1
           str     rf
           ldi     high linenum        ; set line number
           phi     rf
           ldi     low linenum
           plo     rf
           ldi     0
           str     rf
           inc     rf
           str     rf
           ldi     0                   ; set starting address
           phi     ra
           plo     ra
           sep     scall               ; perform second pass
           dw      asmloop
           sep     scall               ; close the file
           dw      o_close

           ldi     high fname2         ; prepare to open output file
           phi     rf
           ldi     low fname2
           plo     rf
           ldi     high fildes         ; point to file descriptor
           phi     rd
           ldi     low fildes
           plo     rd
           ldi     3                   ; flags create, truncate
           plo     r7
           sep     scall               ; attempt to open the file
           dw      o_open
           lbdf    openerr             ; jump if failed to open
           mov     rf,lowmem           ; point to low memory
           lda     rf                  ; need to compute image size
           phi     rc                  ; so retrieve low mem
           lda     rf
           plo     rc
           inc     rf                  ; point to lsb of high mem
           ldn     rf                  ; retrieve it
           str     r2                  ; subtract low mem from it
           glo     rc                  ; get lsb of low mem
           sd
           plo     rc
           dec     rf                  ; point to msb
           ldn     rf                  ; retrieve it
           str     r2                  ; store for subtract
           ghi     rc                  ; msb of low mem
           sdb
           phi     rc
           inc     rc                  ; +1
           ghi     rc                  ; store image size in header
           str     rf
           inc     rf
           glo     rc
           str     rf
           push    rc                  ; save image size
           mov     rf,lowmem           ; point back to header
           ldi     0                   ; 6 bytes to write
           phi     rc
           ldi     6
           plo     rc
           sep     scall               ; write the header
           dw      o_write
           ldi     high lowmem         ; point to low mem
           phi     r9
           ldi     low lowmem
           plo     r9
           lda     r9                  ; get for start of write
           phi     rf
           lda     r9
           plo     rf
           pop     rc                  ; recover image size
           sep     scall               ; write data block
           dw      o_write
           sep     scall               ; close the file
           dw      o_close
           sep     sret                ; and return to caller

passsymb:  lda     rf                  ; get next character
           sep     scall               ; see if alphanumeric
           dw      isalnum
           lbdf    passsymb            ; jump if so
           dec     rf                  ; back to terminating character
           sep     sret                ; return to caller

; ****************************
; *** Move past whitespace ***
; ****************************
trim:      lda     rf                  ; get byte from buffer
           plo     re                  ; save byte
           smi     9                   ; check for tab
           lbz     trim                ; move past tab
           glo     re
           lbz     ltrim0              ; jump if it was zero
           smi     33                  ; get rid of anything space or lower
           lbnf    trim
ltrim0:    dec     rf                  ; point back to character
           sep     sret                ; and return

; Codes:
;     1 - 1 byte code
;     2 - 2 byte code
;     3 - 3 byte code
;     4 - special handling, 
;     5 - nybble mode

; **************************
; *** Process END opcode ***
; **************************
opend:     sep     scall               ; get argument
           dw      getarg
           ldi     high startaddr      ; get address for start
           phi     r8
           ldi     low startaddr
           plo     r8
           ghi     rd                  ; write start address
           str     r8
           inc     r8
           glo     rd
           str     r8
           lbr     opgood              ; done

; **************************
; *** Process ORG opcode ***
; **************************
oporg:     sep     scall               ; get argument
           dw      getarg
           ghi     rd                  ; set as addres
           phi     ra
           glo     rd
           plo     ra
           lbr     opgood

; *************************
; *** Process DS opcode ***
; *************************
opds:      sep     scall               ; get argument
           dw      getarg
           glo     rd                  ; add into address pointer
           str     r2
           glo     ra
           add
           plo     ra
           ghi     rd
           str     r2
           ghi     ra
           adc
           phi     ra
           lbr     opgood
 
; *************************
; *** Process DB opcode ***
; *************************
opdb:      sep     scall               ; move past whitespace
           dw      ltrim
           ldn     rf                  ; get next byte
           smi     27h                 ; check for open quote
           lbz     opdbqt              ; jump if so
           sep     scall               ; get argument
           dw      getarg
           glo     rd                  ; and output low byte
           sep     scall
           dw      output
opdbclqt:  sep     scall               ; move past whitespace
           dw      ltrim
           ldn     rf                  ; check for a comma
           smi     ','
           lbnz    opdbdn              ; jump if not
           inc     rf                  ; move past comma
           lbr     opdb                ; process next byte
opdbdn:    lbr     opgood
opdbqt:    inc     rf                  ; move to next character
opdbqtlp:  lda     rf                  ; get byte
           lbz     opdbdn              ; jump if terminator found
           plo     re                  ; save a copy
           smi     27h                 ; see if closing quote
           lbz     opdbclqt            ; jump if so
           glo     re                  ; get byte
           sep     scall               ; and output it
           dw      output
           lbr     opdbqtlp            ; keep walking through
 
; *************************
; *** Process DW opcode ***
; *************************
opdw:      sep     scall               ; move past whitespace
           dw      ltrim
           ldn     rf                  ; get next byte
           smi     27h                 ; check for open quote
           lbz     opdwqt              ; jump if so
           sep     scall               ; get argument
           dw      getarg
           ghi     rd                  ; and output high byte
           sep     scall
           dw      output
           glo     rd                  ; and output low byte
           sep     scall
           dw      output
opdwclqt:  sep     scall               ; move past whitespace
           dw      ltrim
           ldn     rf                  ; check for a comma
           smi     ','
           lbnz    opdwdn              ; jump if not
           inc     rf                  ; move past comma
           lbr     opdw                ; process next byte
opdwdn:    lbr     opgood
opdwqt:    inc     rf                  ; move to next character
opdwqtlp:  lda     rf                  ; get byte
           lbz     opdwdn              ; jump if terminator found
           plo     re                  ; save a copy
           smi     27h                 ; see if closing quote
           lbz     opdwclqt            ; jump if so
           glo     re                  ; get byte
           sep     scall               ; and output it
           dw      output
           lbr     opdwqtlp            ; keep walking through

; **************************
; *** Process LDN opcode ***
; **************************
opldn:     sep     scall               ; get argument
           dw      getarg
           glo     rd                  ; get low value
           ani     0fh                 ; want only low byte
           lbz     opldner             ; zero is not allowed
           sep     scall               ; output the byte
           dw      output
opgood:    ldi     0                   ; signal success
           shr
           sep     sret                ; return to caller
opldner:   ldi     1                   ; signal an error
           shr
           sep     sret                ; and return to caller

; **************************
; *** Process OUT opcode ***
; **************************
opout:     sep     scall               ; get argument
           dw      getarg
           glo     rd
           lbz     opldner             ; cannot be zero
           smi     8                   ; or greater than 7
           lbdf    opldner
           glo     rd
           ori     060h                ; convert to out instruction
           sep     scall               ; output the byte
           dw      output
           lbr     opgood              ; then signal good

; **************************
; *** Process INP opcode ***
; **************************
opinp:     sep     scall               ; get argument
           dw      getarg
           glo     rd
           lbz     opldner             ; cannot be zero
           smi     8                   ; or greater than 7
           lbdf    opldner
           glo     rd
           adi     8                   ; move to input range
           ori     060h                ; convert to out instruction
           sep     scall               ; output the byte
           dw      output
           lbr     opgood              ; then signal good

ltrim:     lda     rf                  ; get byte from buffer
           lbz     ltrimdn             ; jump if end of string found
           plo     re                  ; check for tab character
           smi     9
           lbz     ltrim
           glo     re
           smi     33                  ; check for space or less
           lbnf    ltrim               ; loop back if space or less
ltrimdn:   dec     rf                  ; move back to prev char
           sep     sret                ; and return

output:    str     ra                  ; store value
           ldi     high pass           ; find out which pass
           phi     r8
           ldi     low pass
           plo     r8
           ldn     r8                  ; get current pass
           lbz     outputps0           ; jump if pass 0
           glo     rd                  ; save consumed registers
           stxd
           ghi     rd
           stxd
           glo     rf
           stxd
           ghi     rf
           stxd
           ldi     high bytecnt        ; need to check byte counter
           phi     rf
           ldi     low bytecnt
           plo     rf
           ldn     rf
           smi     4                   ; have 4 bytes already been output
           lbnz    outputgo            ; jump if not
           ldi     0                   ; reset count
           str     rf
           ldi     high nxtline        ; move pointer to next line
           phi     rf
           ldi     low nxtline
           plo     rf
           sep     scall
           dw      o_msg
outputgo:  ldn     ra                  ; recover byte
           plo     rd                  ; prepare to convert
           ldi     high outbuffer      ; point to output buffer
           phi     rf
           ldi     low outbuffer
           plo     rf 
           sep     scall               ; convert the value
           dw      f_hexout2
           ldi     32                  ; need a space
           str     rf
           inc     rf
           ldi     0                   ; terminate string
           str     rf
           ldi     high outbuffer      ; point to output buffer
           phi     rf
           ldi     low outbuffer
           plo     rf 
           sep     scall               ; display the byte
           dw      o_msg
           ldi     high bytecnt        ; need to increment byte counter
           phi     rf
           ldi     low bytecnt
           plo     rf
           ldn     rf
           adi     1
           str     rf
           irx                         ; recover consumed registers
           ldxa
           phi     rf
           ldxa
           plo     rf
           ldxa
           phi     rd
           ldx
           plo     rd
outputps0: ghi     ra                  ; store address for subtract
           stxd
           glo     ra
           str     r2
           ldi     high lowmem         ; point to low memory storage
           phi     r8
           ldi     low lowmem
           plo     r8
           inc     r8                  ; point to low byte
           ldn     r8                  ; get low byte
           sm                          ; and subtract address
           dec     r8                  ; point to high bytes
           irx
           ldn     r8
           smb
           lbnf    notlow              ; jump if address is not low
           ghi     ra                  ; store new lowmem
           str     r8
           inc     r8
           glo     ra
           str     r8
           dec     r8
notlow:    ghi     ra                  ; store address for subtract
           stxd
           glo     ra
           str     r2
           inc     r8                  ; point to himem value
           inc     r8
           inc     r8                  ; low byte
           ldn     r8                  ; get low byte
           sm                          ; and subtract address
           dec     r8                  ; point to high bytes
           irx
           ldn     r8
           smb
           lbdf    nothigh             ; jump if address is not low
           ghi     ra                  ; store new lowmem
           str     r8
           inc     r8
           glo     ra
           str     r8
           dec     r8
nothigh:   inc     ra                  ; point to next memory location
           sep     sret                ; return to caller

; ***********************************
; *** Check for symbol terminator ***
; *** Returns: DF=1 - terminator  ***
; ***********************************
isterm:    plo     re                  ; save for tests
           smi     '0'                 ; anything below zero is valid
           lbnf    passes              ; jump if below number 0
           smi     10                  ; number digits fail
           lbnf    fails
           smi     7                   ; now good if less than A
           lbnf    passes
           smi     26                  ; uc letters fail
           lbnf    fails
           smi     6                   ; less than lc succeeds
           lbnf    passes
           smi     26                  ; lc letters fail
           lbnf    fails
           lbr     passes              ; all others pass

; **********************************
; *** check D if numeric         ***
; *** Returns DF=1 - numeric     ***
; ***         DF=0 - non-numeric ***
; **********************************
isnum:     smi     '0'                 ; check for below zero
           lbnf    fails               ; jump if not
           smi     10                  ; see if above
           lbdf    fails               ; jump if so
           lbr     passes              ; otherwise passes

; ********************************
; *** See if D is alphabetic   ***
; *** Returns DF=0 - not alpha ***
; ***         DF=1 - is alpha  ***
; ********************************
isalpha:   plo     re                  ; save copy of do
           smi     'A'                 ; check uc letters
           lbnf    fails               ; jump if below A
           smi     27                  ; check upper range
           lbnf    passes              ; jump if valid
           glo     re                  ; recover character
           smi     'a'                 ; check lc letters
           lbnf    fails               ; jump if below A
           smi     27                  ; check upper range
           lbnf    passes              ; jump if valid
fails:     adi     0                   ; indicate test failure
           glo     re                  ; recover value
           sep     sret                ; and return
passes:    smi     0                   ; indicate test success
           glo     re                  ; recover value
           sep     sret                ; and return

; *****************************************
; *** See if D is valid label character ***
; *** Returns: DF=0 - not valid         ***
; ***          DF=1 - is valid          ***
; *****************************************
isalnum:   plo     re                  ; keep copy of D
           smi     '_'                 ; underscore is valid
           lbz     passes              ; jump if so
           glo     re                  ; recover character
           sep     scall               ; check if numeric
           dw      isnum
           lbdf    passes              ; jump if numeric
           sep     scall               ; check for alpha
           dw      isalpha
           lbdf    passes              ; jump if alpha
           lbr     fails               ; otherwise fails

; **********************************
; *** check D if hex             ***
; *** Returns DF=1 - hex         ***
; ***         DF=0 - non-hex     ***
; **********************************
ishex:     sep     scall               ; see if it is numeric
           dw      isnum
           plo     re                  ; keep a copy
           lbdf    ishexyes            ; jump if it is numeric
           smi     'A'                 ; check for below uppercase a
           lbnf    ishexno             ; value is not hex
           smi     6                   ; check for less then 'G'
           lbnf    ishexyes            ; jump if so
           glo     re                  ; recover value
           smi     'a'                 ; check for lowercase a
           lbnf    ishexno             ; jump if not
           smi     6                   ; check for less than 'g'
           lbnf    ishexyes            ; jump if so
ishexno:   adi     0                   ; signal not hex
           glo     re                  ; recover number
           sep     sret                ; and return to caller
ishexyes:  smi     0                   ; indicate a yes
           glo     re                  ; recover number
           sep     sret                ; and return to caller

; *******************************
; *** Convert hex D to binary ***
; *** Returns: D - value      ***
; *******************************
hexbin:    stxd                        ; save value
           sep     scall               ; see if numeric
           dw      isnum
           lbnf    hexbin2             ; jump if alpha
           irx                         ; recover value
           ldx
           smi     '0'                 ; convert to binary
           sep     sret                ; and return to caller
hexbin2:   irx                         ; recover value
           ldx
           ani     0dfh                ; force to uppercase
           smi     55                  ; convert to binary
           sep     sret                ; and return to caller

; ***********************************************
; *** identify symbol as decimal, hex, or non ***
; *** RF - pointer to symbol                  ***
; *** Returns: D=0 - decimal number           ***
; ***          D=1 - hex number               ***
; ***          D=3 - non numeric              *** 
; ***          RD  - number                   ***
; ***          RF  - first char after symbol  ***
; ***********************************************
identify:  glo     rf                  ; save position
           stxd
           ghi     rf
           stxd
           ldn     rf                  ; get first byte
           sep     scall               ; must be numeric
           dw      isnum
           lbnf    iderr               ; jump if not
idlp1:     lda     rf                  ; get next byte
           sep     scall               ; see if terminator
           dw      isterm              ; see if terminator was found
           lbdf    isdec               ; number was decimal
           sep     scall               ; see if character was numeric
           dw      isnum
           lbdf    idlp1               ; jump if so
           dec     rf                  ; point back to char
idlp2:     lda     rf                  ; get next byte
           sep     scall               ; see if terminator
           dw      isterm
           lbdf    iderr               ; jump if terminator found before h
           sep     scall               ; check for hex char
           dw      ishex
           lbdf    idlp2               ; loop back if so
           plo     re                  ; save char
           smi     'H'                 ; check for H
           lbz     idhex               ; jump if found
           glo     re                  ; check for lc h
           smi     'h'
           lbz     idhex
iderr:     irx                         ; number is an error, recover RF
           ldxa
           phi     rf
           ldx
           plo     rf
           ldi     3                   ; signal non-numeric
           sep     sret                ; return to caller
idhex:     ldn     rf                  ; byte following h must be a terminator
           sep     scall
           dw      isterm
           lbnf    iderr               ; error if no terminator
           irx                         ; recover beginning of number
           ldxa
           phi     rf
           ldx
           plo     rf
           sep     scall               ; convert ascii hex to binary
           dw      f_hexin
           inc     rf                  ; move past h
           ldi     0                   ; signal number was hex
           sep     sret                ; and return to caller
isdec:     irx                         ; recover beginning of number
           ldxa
           phi     rf
           ldx
           plo     rf
           sep     scall               ; convert ascii decimal to binary
           dw      f_atoi
           ldi     0                   ; signal number was decimal
           sep     sret                ; and return to caller

; ***************************************
; *** Get argument from assembly line ***
; *** RF - line after opcode          ***
; *** RB - pointer to symbol table    ***
; *** Returns: RD - value of arg      ***
; ***          DF=0 - no error        ***
; ***          DF=1 - error           ***
; ***             D=1 - no arg        ***
; ***             D=2 - inv number    ***
; ***             D=3 - inv label     ***
; ***************************************
getarg:     sep     scall               ; tokenize next input
            dw      tokenize
            lbdf    argerr              ; jump on tokenization error
            glo     rf                  ; save buffer pointer
            stxd
            ghi     rf
            stxd
            sep     scall               ; call the evaluator
            dw      reduce
            irx                         ; recover buffer pointer
            ldxa
            phi     rf
            ldx
            plo     rf
            lbdf    argerr              ; jump on error
            adi     0                   ; signal success
            sep     sret                ; return to caller
      
argerr:     smi     0                   ; signal an error
            sep     sret                ; and return

;           sep     scall               ; move past whitespace
;           dw      ltrim
;           ldn     rf                  ; get first argument byte
;           lbz     getarg1             ; jump if at terminator
;           smi     27h                 ; check for character constant
;           lbz     charconst           ; jump if so
;           ldn     rf                  ; recover number
;           sep     scall               ; see if it is numeric
;           dw      isnum
;           lbnf    argnum              ; jump if argument is numeric
;           ldn     rf                  ; check for possible register
;           smi     'R'
;           lbz     argreg              ; might be
;           ldn     rf                  ; check lowercase r as well
;           smi     'r'
;           lbz     argreg
;notreg:    sep     scall               ; search for symbol
;           dw      getsymbol
;           lbdf    getnolab            ; jump if label was not valid
;           ldi     0                   ; signal success
;           shr
;           sep     sret                ; return to caller
;getnolab:  ldi     0                   ; invalid labels are zero
;           phi     rd
;           plo     rd
;           ldi     7                   ; signal invalid label
;           shr
;           sep     sret                ; and return to caller
;
;argreg:    inc     rf                  ; point to next byte
;           ldn     rf                  ; retrieve it
;           sep     scall               ; see if is hex
;           dw      ishex
;           lbdf    argno               ; jump if not hex
;           inc     rf                  ; get following byte
;           ldn     rf
;           dec     rf                  ; keep pointer pointing at 2nd byte
;           smi     33                  ; must be a space or less
;           lbdf    argno               ; nope, it was not a register
;           ldn     rf                  ; retrieve register number
;           sep     scall              
;           dw      hexbin              ; convert to binary
;           plo     rd                  ; place into return register
;           ldi     0                   ; high byte is zero
;           phi     rd
;           shr                         ; signal success
;           sep     sret                ; return to caller
;argno:     dec     rf                  ; point back to first byte
;           lbr     notreg              ; and process as a label
;
;argnum:    sep     scall               ; identify number
;           dw      identify
;           smi     3                   ; was it non-numeric
;           lbz     argnumer            ; jump if non-numeric
;           ldi     0                   ; signal valid
;           shr
;           sep     sret                ; and return to caller
;argnumer:  ldi     5                   ; signal invalid number
;           shr
;           sep     sret                ; and return to caller
;          
;
;getarg1:   ldi     0                   ; not found, set value as zero
;           phi     rd
;           plo     rd
;           ldi     3                   ; set no arg error
;           shr
;           sep     sret                ; return to caller
;
;charconst: inc     rf                  ; point to character
;           lda     rf                  ; get character
;           plo     rd                  ; store for return
;           ldi     0                   ; high byte is zero
;           phi     rd
;           lda     rf                  ; need to check for close quote
;           smi     27h
;           lbnz    constno             ; jump if not valid
;           adi     0                   ; signal valid
;           sep     sret                ; and return to caller
;constno:   dec     rf                  ; move pointer back
;           dec     rf
;           dec     rf
;           ldi     5                   ; signal error
;           shr
;           sep     sret                ; and return

; ************************************
; *** Assemble next line           ***
; *** RF - line to assemble        ***
; *** RA - Current address         ***
; *** RB - pointer to symbol table ***
; *** Returns: DF=0 - success      ***
; ***          DF=1 - failed       ***
; ************************************
asm:       ldi     high curaddr        ; set current address
           phi     r8
           ldi     low curaddr
           plo     r8
           ghi     ra
           str     r8
           inc     r8
           glo     ra
           str     r8
           ldi     high pass           ; get current pass
           phi     r8
           ldi     low pass
           plo     r8
           ldn     r8
           lbz     asmpass0            ; can only get on second pass
           glo     rf                  ; save line position
           stxd
           ghi     rf
           stxd
; *** First show line number ***
           ldi     high linenum        ; point to line number
           phi     rf
           ldi     low linenum
           plo     rf
           lda     rf                  ; retrieve line number
           phi     rd                  ; place into RD for conversion
           ldn     rf
           plo     rd
           ldi     high outbuffer      ; get address of buffer
           phi     rf
           ldi     low outbuffer
           plo     rf
           sep     scall               ; convert number for output
           dw      f_uintout
           ldi     ' '                 ; space after number
           str     rf                  ; store into buffer
           inc     rf
           ldi     0                   ; now terminator
           str     rf
           ldi     high outbuffer      ; get address of buffer
           phi     rf
           ldi     low outbuffer
           plo     rf
           ldi     6                   ; need 6 characters
           plo     re
blloop1:   lda     rf                  ; get byte from buffer
           lbz     blgo1               ; jump if found terminator
           dec     re                  ; decrement counter
           lbr     blloop1             ; and keep looking
blgo1:     glo     re                  ; check for end of spaces
           lbz     blgo2               ; jump if done
           glo     re                  ; save count
           stxd
           ldi     ' '                 ; otherwise output a space
           sep     scall 
           dw      o_type
           irx                         ; recover count
           ldx
           plo     re
           dec     re                  ; decrement count
           lbr     blgo1               ; loop until done
blgo2:     ldi     high outbuffer      ; get address of buffer
           phi     rf
           ldi     low outbuffer
           plo     rf
           sep     scall               ; now display it
           dw      o_msg

; *** Now show address ***
           ldi     high bytecnt        ; zero byte count for line
           phi     rf
           ldi     low bytecnt
           plo     rf
           ldi     0
           str     rf
           ghi     ra                  ; transfer address to RD
           phi     rd
           glo     ra
           plo     rd
           ldi     high outbuffer      ; get address of buffer
           phi     rf
           ldi     low outbuffer
           plo     rf
           sep     scall               ; convert address
           dw      f_hexout4
           ldi     32                  ; need a space
           str     rf
           inc     rf
           ldi     0                   ; place terminator
           str     rf
           ldi     high outbuffer      ; get address of buffer
           phi     rf
           ldi     low outbuffer
           plo     rf
           sep     scall               ; display the address
           dw      o_msg
           irx                         ; recover line buffer
           ldxa
           phi     rf
           ldx
           plo     rf
asmpass0:  ldn     rf                  ; get first byte from line
           smi     33                  ; check for space or less
           lbnf    asm2                ; jump if no labels
           ghi     rf                  ; make a copy
           phi     r9
           glo     rf
           plo     r9
asmlbl1:   ldn     rf                  ; get byte from label
           smi     ':'                 ; check for end of label
           lbz     asmlble             ; found end
           ldn     rf                  ; get another byte
           smi     33                  ; check for space or less
           lbnf    asmlble             ; found end
           inc     rf                  ; point to next byte
           lbr     asmlbl1             ; and keep looking
asmlble:   ldi     0                   ; terminate the label
           str     rf
           glo     rf                  ; save positoin
           stxd
           ghi     rf
           stxd
           ghi     r9                  ; get start of label
           phi     rf
           glo     r9
           plo     rf
           sep     scall               ; add the symbol
           dw      addsym
           irx                         ; recover line pointer
           ldxa
           phi     rf
           ldx
           plo     rf
           ldi     ':'
           str     rf
           inc     rf
asm2:      ldi     high insttab        ; point to opcode table
           phi     r9
           ldi     low insttab
           plo     r9
           sep     scall               ; move past leading spaces
           dw      ltrim
asmlp1:    sep     scall               ; check for opcode match
           dw      chkentry
           lbnf    asmfnd              ; jump if entry was found
           inc     r9                  ; point to next opcode entry
           inc     r9
           inc     r9
           ldn     r9                  ; get byte from table
           lbnz    asmlp1              ; loop back if not end of table
           ldi     1                   ; signal an error
asmret:    shr
           sep     sret                ; and return to caller
asmgood:   ldi     0                   ; signal success
           lbr     asmret
asmfnd:    lda     r9                  ; get instruction type
           smi     1                   ; see if 1 byte code
           lbnz    asmnot1             ; jump if not
           lda     r9                  ; get opcode
           sep     scall               ; output the byte
           dw      output
           lbr     asmgood             ; then return
asmnot1:   smi     1                   ; check for 2 byte instruction
           lbnz    asmnot2             ; jump if not
           lda     r9                  ; get opcode
           sep     scall               ; and output it
           dw      output
           sep     scall               ; get argument
           dw      getarg
           lbdf    asmerror            ; jump if error in argument
           glo     rd                  ; get low value
           sep     scall               ; and output it
           dw      output
           lbr     asmgood             ; then return
asmnot2:   smi     1                   ; check for 3 byte instruction
           lbnz    asmnot3             ; jump if not
           lda     r9                  ; get opcode
           sep     scall               ; and output it
           dw      output
           sep     scall               ; get argument
           dw      getarg
           lbdf    asmerror            ; jump if error in argument
           ghi     rd                  ; get high value
           sep     scall               ; and output it
           dw      output
           glo     rd                  ; get low value
           sep     scall               ; and output it
           dw      output
           lbr     asmgood             ; then return
asmnot3:   smi     1                   ; check for special handling
           lbnz    asmnot4             ; jump if not
           ldi     high jump           ; get jump vector
           phi     r7
           ldi     low jump
           plo     r7
           inc     r7                  ; point to jump destination
           lda     r9                  ; get handler
           str     r7                  ; and write into jump address
           inc     r7
           ldn     r9
           str     r7
jump:      lbr     0                   ; jump to special routine
asmnot4:   lda     r9                  ; get opcode
           stxd                        ; and save it
           sep     scall               ; get argument
           dw      getarg
           irx                         ; point to opcode
           lbdf    asmerror            ; jump if error in argument
           glo     rd                  ; get low value
           ani     0fh                 ; keep only low nybble
           or                          ; and or with register nybble
           sep     scall               ; and output it
           dw      output
           lbr     asmgood             ; then return
asmerror:  ldi     high errmsg         ; point to error message
           phi     rf
           ldi     low errmsg
           plo     rf
           sep     scall
           dw      o_msg
           ldi     0
           shr
           sep     sret
           


; ***********************************************
; *** Check to see if a table entry matches   ***
; *** RF - buffer                             ***
; *** R9 - table entry                        ***
; *** Returns: R9 - first byte following name ***
; ***          DF=0 - entry found             ***
; ***          DF=1 - entry not foune         ***
; ***          RF=orig on failure             ***
; ***          otherwise first byte after     ***
; ***********************************************
chkentry:  glo      rf                 ; save buffer position
           stxd
           ghi      rf
           stxd
chkloop:   lda      r9                 ; get byte from table entry
           plo      re                 ; keep a copy
           ani      128                ; see if high bit is set
           lbnz     chklst             ; jump if last character
           lda      rf                 ; get byte from buffer
           str      r2                 ; put in memory for compare
           glo      re                 ; recover table byte
           sm                          ; compare values
           lbz      chkloop            ; loop back if a match
chklp2:    lda      r9                 ; need to find end of entry
           ani      128
           lbz      chklp2             ; loop until end found
chkno:     ldi      1                  ; signal entry not found
chkend:    shr
           irx                         ; recover buffer position
           ldxa
           phi      rf
           ldx
           plo      rf
           sep      sret               ; and return to caller
chklst:    lda      rf                 ; get byte from buffer
           str      r2                 ; put in memory for compare
           glo      re                 ; recover table byte
           ani      07fh               ; strip high bit
           sm                          ; compare with buffer byte
           lbnz     chkno              ; jump if no match
           ldn      rf                 ; load next byte from buffer
           smi      33                 ; must be space or less
           lbdf     chkno              ; jump if not space or less
           ldi      0                  ; signal success
           shr
           irx                         ; remove RF from stack
           irx
           sep      sret               ; and return to caller

; ***********************************
; *** Process over the input file ***
; *** RD - file descriptor        ***
; ***********************************
asmloop:   ldi     high buffer         ; point to buffer
           phi     rf
           ldi     low buffer
           plo     rf
           sep     scall               ; read next line
           dw      readln
           lbdf    asmloopdn           ; jump if end of file reached
           glo     rc                  ; get character count
           lbz     asmloop             ; jump if at end of file
           mov     rf,buffer           ; convert to upper case
           sep     scall
           dw      touc
asmnodsp:  ldi     high buffer         ; point back to input line
           phi     rf
           ldi     low buffer
           plo     rf
           glo     rd                  ; save descriptor
           stxd
           ghi     rd
           stxd
           sep     scall               ; assemble the line
           dw      asm
           ldi     high pass           ; get pass
           phi     rf
           ldi     low pass
           plo     rf
           ldn     rf
           lbz     asmlpps0            ; jump if on first pass
           shrc                        ; save DF
           stxd
           ldi     high bytecnt        ; need output byte count
           phi     rf
           ldi     low bytecnt
           plo     rf
           ldn     rf
outbloop:  stxd                        ; save copy of count
           smi     4                   ; have 4 bytes been output
           lbz     outlpdn             ; jump if so
           ldi     high outbuffer      ; point back to input line
           phi     rf
           ldi     low outbuffer
           plo     rf
           ldi     32                  ; need 3 spaces
           str     rf
           inc     rf
           str     rf
           inc     rf
           str     rf
           inc     rf
           ldi     0                   ; and a terminator
           str     rf
           inc     rf
           dec     rf                  ; back to beginning of buffer
           dec     rf
           dec     rf
           dec     rf
           sep     scall               ; display the blanks
           dw      o_msg
           irx                         ; recover count
           ldx
           adi     1                   ; add 1
           lbr     outbloop            ; loop back til done
outlpdn:   irx                         ; remove D from stack
           ldi     high buffer         ; point back to input line
           phi     rf
           ldi     low buffer
           plo     rf
           sep     scall               ; display it
           dw      o_msg
           ldi     high crlf           ; display a cr/lf
           phi     rf
           ldi     low crlf
           plo     rf
           sep     scall               ; display it
           dw      o_msg
           irx                         ; recover DF
           shl
asmlpps0:  lbnf    asmnoerr            ; jump if no error happened
           ldi     high errmsg         ; otherwise display error message
           phi     rf
           ldi     low errmsg
           plo     rf
           sep     scall
           dw      o_msg
asmnoerr:  irx                         ; recover descripter
           ldxa
           phi     rd
           ldx
           plo     rd
           lbr     asmloop             ; loop back for next line
asmloopdn: sep     sret                ; return to caller


readln:    glo     rf                  ; save buffer position
           stxd
           ghi     rf
           stxd
           ldi     high linenum        ; point to line number
           phi     rf
           ldi     low linenum
           plo     rf
           inc     rf                  ; point to lsb
           ldn     rf                  ; get it
           adi     1                   ; add 1
           str     rf                  ; and save it
           dec     rf                  ; point to msb
           ldn     rf                  ; retrieve it
           adci    0                   ; propagate carry
           str     rf
           irx                         ; recover rf
           ldxa
           phi     rf
           ldx
           plo     rf
           ldi     0                   ; set byte count
           phi     rc
           plo     rc
readln1:   sep     scall               ; read a byte
           dw      readbyte
           lbdf    readlneof           ; jump on eof
           plo     re                  ; keep a copy
           smi     9                   ; check for tab
           lbz     readln2             ;jump if so
           glo     re
           smi     32                  ; look for anything below a space
           lbnf    readln1
readln2:   glo     re                  ; recover byte
           str     rf                  ; store into buffer
           inc     rf                  ; point to next position
           inc     rc                  ; increment character count
           sep     scall               ; read next byte
           dw      readbyte
           lbdf    readlneof           ; jump if end of file
           plo     re                  ; keep a copy of read byte
           smi     9
           lbz     readln2
           glo     re
           smi     32                  ; make sure it is positive
           lbdf    readln2             ; loop back on valid characters
           ldi     0                   ; signal valid read
readlncnt: shr                         ; shift into DF
           ldi     0
           str     rf
           sep     sret                ; and return to caller
readlneof: ldi     1                   ; signal eof
           lbr     readlncnt

readbyte:
;          glo     rf
;          stxd
;          ghi     rf
;          stxd
           glo     rc
           stxd
           ghi     rc
           stxd
;          ldi     high char
;          phi     rf
;          ldi     low char
;          plo     rf
           ldi     0
           phi     rc
           ldi     1
           plo     rc
           sep     scall
           dw      o_read
           glo     rc
           dec     rf
           lbz     readbno
           ldn     rf                  ; get byte
           smi     01ah                ; check for XMODEM eof
           lbz     readbno             ; jump if EOF detected
           ldi     0
readbcnt:  shr
;          ldi     high char
;          phi     rf
;          ldi     low char
;          plo     rf
           ldn     rf
           plo     re
           irx
           ldxa
           phi     rc
;          ldxa
           ldx
           plo     rc
;          ldxa
;          phi     rf
;          ldx
;          plo     rf
           glo     re
           sep     sret
readbno:   ldi     1
           lbr     readbcnt

; **************************************
; *** Scan symbol table for a symbol ***
; *** RF - symbol to search for      ***
; *** Returns: RD - value            ***
; ***          DF=0 - entry found    ***
; ***          DF=1 - not found      ***
; **************************************
getsymbol: ldn     rf                  ; get first byte
           smi     '$'                 ; check for dollar sign
           lbnz    notdollar           ; jump if not
           ldi     high curaddr        ; point to current address
           phi     rd
           ldi     low curaddr
           plo     rd
           lda     rd                  ; get high byte
           plo     re                  ; set aside
           ldn     rd                  ; get low byte
           plo     rd                  ; set rd to current address
           glo     re
           phi     rd
           inc     rf                  ; move past $ symbol
           ldi     0                   ; signal success
           shr
           sep     sret                ; return to caller
notdollar: ldi     high pass           ; get current pass
           phi     r8
           ldi     low pass
           plo     r8
           ldn     r8
           lbnz    getsym1             ; can only get on second pass
           sep     scall               ; move pointer past symbol
           dw      passsymb
           ldi     0                   ; first pass symbol is zero
           phi     rd
           plo     rd
           shr                         ; and symbol is assumed to exist
           sep     sret                ; otherwise return to caller
getsym1:   ldi     high symtab         ; point to symbol table
           phi     r8
           ldi     low symtab
           plo     r8
getsymlp:  ldn     r8                  ; get byte from symbol table
           lbnz    getsymgo            ; go if still valid symbols
           sep     scall               ; move pointer past symbol
           dw      passsymb
           ldi     1                   ; signal not found
           shr
           sep     sret                ; and return to caller
getsymgo:  glo     rf                  ; save user search
           stxd
           ghi     rf
           stxd
getsymlp2: lda     r8                  ; get byte from symbol table
           str     r2                  ; keep a copy
           ani     128                 ; is high bit set
           lbnz    getsymlst           ; jump if so
           lda     rf                  ; get byte from search
           sm                          ; compare
           lbz     getsymlp2           ; jump if a match
getsymnxt: lda     r8                  ; get byte from sybol table
           ani     128                 ; check for final byte
           lbz     getsymnxt           ; loop back if not
getsymnls: inc     r8                  ; point to next symbol
           inc     r8
           irx                         ; recover user search
           ldxa
           phi     rf
           ldx
           plo     rf
           lbr     getsymlp            ; and check next symbol
getsymlst: ldn     r2                  ; recover byte
           ani     07fh                ; strip high bit 
           str     r2                  ; put back in memory
           lda     rf                  ; get byte from user search
           sm                          ; are they the same
           lbnz    getsymnls           ; jump if not
           lda     rf                  ; get next byte
           plo     re                  ; keep a copy
           smi     '.'                 ; see if it is a dot
           lbz     getsymdot           ; jump if so
           glo     re                  ; recover byte
           smi     33                  ; must be space or less
           lbdf    getsymnls           ; jump if not
           irx                         ; recover user search
           ldxa
           phi     rf
           ldx
           plo     rf
           sep     scall               ; move pointer past symbol
           dw      passsymb
           ldi     0                   ; indicate use symbol as is
           plo     re
getsymcnt: lda     r8                  ; get symbol value
           phi     rd
           lda     r8
           plo     rd
           glo     re                  ; see if need high in low
           smi     '1'
           lbz     getsymsw            ; yep
           ldi     0                   ; signal success
           shr
           sep     sret                ; and return to caller
getsymsw:  ghi     rd                  ; move high value to low
           plo     rd
           ldi     0                   ; zero high byte
           shr
           phi     rd
           sep     sret                ; return to caller
getsymdot: ldn     rf                  ; get byte following dot
           plo     re                  ; store into re
           irx                         ; recover user search
           ldxa
           phi     rf
           ldx
           plo     rf
           lbr     getsymcnt           ; then continue

; *******************
; *** Add symbol  ***
; *** RF - symbol ***
; *** RA - value  ***
; *******************
addsym:    ldi     high pass           ; get current pass
           phi     r8
           ldi     low pass
           plo     r8
           ldn     r8
           lbz     addsym1             ; can only add on first pass
           sep     sret                ; otherwise return to caller
addsym1:   ldi     high lastsym        ; get address of last symbol
           phi     r8
           ldi     low lastsym
           plo     r8
           lda     r8                  ; get high byte for next symbol
           plo     re                  ; keep it
           ldn     r8                  ; get low byte
           plo     r8                  ; set r8 to next symbol address
           glo     re
           phi     r8
addsymlp:  lda     rf                  ; get byte from symbol
           plo     re                  ; keep a copy
           smi     33                  ; check for space or less
           lbnf    addsym2             ; jump if end of symbol reached
           glo     re                  ; recover byte
           str     r8                  ; store into symbol table
           inc     r8                  ; point to next position
           lbr     addsymlp            ; loop until done copying name
addsym2:   dec     r8                  ; point back to previous char
           ldn     r8                  ; retrieve it
           ori     128                 ; set high bit
           str     r8                  ; and put back
           inc     r8                  ; point to value field
           ghi     ra                  ; store value
           str     r8
           inc     r8
           glo     ra
           str     r8
           inc     r8
           ldi     0                   ; end of table marker
           str     r8
           ldi     high lastsym        ; need to store new end value
           phi     rf
           ldi     low lastsym
           plo     rf
           ghi     r8
           str     rf
           inc     rf
           glo     r8
           str     rf
           sep     sret                ; return to caller
           
; **************************************************
; *** Functions for processing expression tokens ***
; **************************************************

; ***************************************
; *** Delete a token                  ***
; *** R8 - address of token to delete ***
; ***************************************
tkn_del:   glo     r8                  ; save token address
           stxd
           ghi     r8
           stxd
           glo     r9                  ; save token address
           stxd
           ghi     r9
           stxd
tkn_dellp: ldn     r8                  ; get token type
           lbz     tkn_deldn           ; jump if no more tokens
           glo     r8                  ; setup pointer to next token
           adi     3                   ; 3 bytes up
           plo     r9
           ghi     r8                  ; high byte as well
           adci    0
           phi     r9
           ldi     3                   ; need to copy 3 bytes
           plo     re                  ; use low RE for counter
tkn_del2:  lda     r9                  ; get byte from next token
           str     r8                  ; and store into current token
           inc     r8
           dec     re                  ; decrement count
           glo     re                  ; and check
           lbnz    tkn_del2            ; loop back if more to copy
           lbr     tkn_dellp           ; loop until all tokens copied
tkn_deldn: irx                         ; recover token pointer
           ldxa
           phi     r9
           ldxa
           plo     r9
           ldxa
           phi     r8
           ldx
           plo     r8
           dec     rc                  ; decrement token counter
           sep     sret                ; return to caller

; *********************************
; *** Get 2nd token             ***
; *** D - Offset to 2nd token   ***
; *** Returns:  D - Token type  ***
; ***          RD - Token value ***
; ***          DF=1 - past end  ***
; *********************************
tkn_get_2: str     r2                  ; store offset for addition
           glo     r8                  ; lsb of current address
           add                         ; add offset
           plo     rd
           ghi     r8                  ; propagate to high byte
           adci    0
           phi     rd
           lda     rd                  ; get token type
           str     r2                  ; save for a moment
           lda     rd                  ; msb of value
           plo     re                  ; save for a moment
           ldn     rd                  ; lsb of value
           plo     rd                  ; set value
           glo     re
           phi     rd
           ldn     r2                  ; recover tokent type
           adi     0                   ; signal no error DF=0
           sep     sret                ; return to caller
           
; *******************************************
; *** Setup arguments for 2-arg functions ***
; *** R8 - function token                 ***
; *** Returns: R8 - lsb of first arg      ***
; ***          RD - Value of second arg   ***
; ***          DF=1 - invalid argument    ***
; *******************************************
tkn_arg:   dec     r8                  ; point to first number
           dec     r8
           dec     r8
           ldn     r8                  ; get token type
           smi     1                   ; must be a number
           lbnz    tkn_arger           ; jump on error
           ldi     6                   ; next token is 6 bytes away
           sep     scall               ; get the token
           dw      tkn_get_2
           smi     1                   ; token must be a number
           lbnz    tkn_arger           ; jump if not
           inc     r8                  ; point to lsb of first arg
           inc     r8
           adi     0                   ; signal no error
           sep     sret                ; and return
tkn_arger: smi     0                   ; signal error
           sep     sret                ; and return

; ************************************
; *** Add 2 tokens                 ***
; *** R8 - address of token to add ***
; *** Returns: DF=1 error          ***
; ************************************
tkn_add:   sep     scall               ; setup arguments
           dw      tkn_arg
           lbdf    tkn_error           ; jump if invalid arguments
           glo     rd                  ; and add numbers together
           str     r2
           ldn     r8
           add
           str     r8
           dec     r8
           ghi     rd                  ; now msb
           str     r2
           ldn     r8
           adc
           str     r8
del_2:     inc     r8                  ; point to operator token
           inc     r8
           sep     scall               ; delete next 2 tokens
           dw      tkn_del
           sep     scall
           dw      tkn_del
           adi     0                   ; indicate no error
           sep     sret                ; and return to caller
tkn_error: smi     0                   ; set DF to indicate an error
           sep     sret                ; and return to caller

; *********************************
; *** Multiply 2 tokens         ***
; *** R8 - address of mul token ***
; *** Returns: DF=1 error       ***
; *********************************
tkn_mul:   sep     scall               ; setup arguments
           dw      tkn_arg
           lbdf    tkn_error           ; jump if invalid arguments
           glo     rf                  ; save consumed registers
           stxd
           ghi     rf
           stxd
           glo     rb
           stxd
           ghi     rb
           stxd
           glo     rc
           stxd
           dec     r8                  ; point to msb of first arg
           lda     r8                  ; copy arg value into RF
           phi     rf
           ldn     r8
           plo     rf
           sep     scall               ; call bios multiply
           dw      f_mul16
           glo     rb                  ; save result
           str     r8
           dec     r8
           ghi     rb
           str     r8
           irx                         ; recover consumed registers
           ldxa
           plo     rc
           ldxa
           phi     rb
           ldxa
           plo     rb
           ldxa
           phi     rf
           ldx
           plo     rf
           lbr     del_2               ; delete used tokens

; *********************************
; *** Multiply 2 tokens         ***
; *** R8 - address of mul token ***
; *** Returns: DF=1 error       ***
; *********************************
tkn_div:   sep     scall               ; setup arguments
           dw      tkn_arg
           lbdf    tkn_error           ; jump if invalid arguments
           glo     rf                  ; save consumed registers
           stxd
           ghi     rf
           stxd
           glo     r8
           stxd
           ghi     r8
           stxd
           glo     r9
           stxd
           ghi     r9
           stxd
           glo     rb
           stxd
           ghi     rb
           stxd
           glo     rc
           stxd
           dec     r8                  ; point to msb of first arg
           lda     r8                  ; copy arg value into RF
           phi     rf
           ldn     r8
           plo     rf
           sep     scall               ; call bios multiply
           dw      f_div16
           glo     rb                  ; save result
           plo     rd
           ghi     rb
           phi     rd
modcont:   irx                         ; recover consumed registers
           ldxa
           plo     rc
           ldxa
           phi     rb
           ldxa
           plo     rb
           ldxa
           phi     r9
           ldxa
           plo     r9
           ldxa
           phi     r8
           ldxa
           plo     r8
           ldxa
           phi     rf
           ldx
           plo     rf
           glo     rd                  ; save result
           str     r8
           dec     r8
           ghi     rd
           str     r8
           lbr     del_2               ; delete used tokens

; *********************************
; *** Mod 2 tokens              ***
; *** R8 - address of mul token ***
; *** Returns: DF=1 error       ***
; *********************************
tkn_mod:   sep     scall               ; setup arguments
           dw      tkn_arg
           lbdf    tkn_error           ; jump if invalid arguments
           glo     rf                  ; save consumed registers
           stxd
           ghi     rf
           stxd
           glo     r8
           stxd
           ghi     r8
           stxd
           glo     r9
           stxd
           ghi     r9
           stxd
           glo     rb
           stxd
           ghi     rb
           stxd
           glo     rc
           stxd
           dec     r8                  ; point to msb of first arg
           lda     r8                  ; copy arg value into RF
           phi     rf
           ldn     r8
           plo     rf
           sep     scall               ; call bios multiply
           dw      f_div16
           glo     rf                  ; save result
           plo     rd
           ghi     rf
           phi     rd
           lbr     modcont             ; continue cleanup

; ************************************
; *** Sub 2 tokens                 ***
; *** R8 - address of token to add ***
; *** Returns: DF=1 error          ***
; ************************************
tkn_sub:   sep     scall               ; setup arguments
           dw      tkn_arg
           lbdf    tkn_error           ; jump if arguments invalid
           glo     rd                  ; subtract 2nd number from first
           str     r2
           ldn     r8
           sm
           str     r8
           dec     r8
           ghi     rd                  ; now msb
           str     r2
           ldn     r8
           smb
           str     r8
           lbr     del_2               ; now delete next 2 tokens

; ************************************
; *** Add 2 tokens                 ***
; *** R8 - address of token to add ***
; *** Returns: DF=1 error          ***
; ************************************
tkn_and:   sep     scall               ; setup arguments
           dw      tkn_arg
           lbdf    tkn_error           ; jump if arguments invalid
           glo     rd                  ; and 2nd number with first
           str     r2
           ldn     r8
           and
           str     r8
           dec     r8
           ghi     rd                  ; now msb
           str     r2
           ldn     r8
           and
           str     r8
           lbr     del_2               ; now delete next 2 tokens

; ************************************
; *** Xor 2 tokens                 ***
; *** R8 - address of token to add ***
; *** Returns: DF=1 error          ***
; ************************************
tkn_xor:   sep     scall               ; setup arguments
           dw      tkn_arg
           lbdf    tkn_error           ; jump if arguments invalid
           glo     rd                  ; and 2nd number with first
           str     r2
           ldn     r8
           xor
           str     r8
           dec     r8
           ghi     rd                  ; now msb
           str     r2
           ldn     r8
           xor
           str     r8
           lbr     del_2               ; now delete next 2 tokens

; ************************************
; *** Or 2 tokens                  ***
; *** R8 - address of token to add ***
; *** Returns: DF=1 error          ***
; ************************************
tkn_or:    sep     scall               ; setup arguments
           dw      tkn_arg
           lbdf    tkn_error           ; jump if arguments invalid
           glo     rd                  ; and 2nd number with first
           str     r2
           ldn     r8
           or
           str     r8
           dec     r8
           ghi     rd                  ; now msb
           str     r2
           ldn     r8
           or
           str     r8
           lbr     del_2               ; now delete next 2 tokens

; ************************************
; *** shr 2 tokens                 ***
; *** R8 - address of token to add ***
; *** Returns: DF=1 error          ***
; ************************************
tkn_shr:   sep     scall               ; setup arguments
           dw      tkn_arg
           lbdf    tkn_error           ; jump if arguments invalid
tkn_shrlp: dec     r8                  ; point r8 at msb
           glo     rd                  ; see if more shifts needed
           lbz     del_2               ; jump if done
           ldn     r8                  ; get msb
           shr                         ; and shift it
           str     r8                  ; put it back
           inc     r8                  ; point to lsb
           ldn     r8                  ; get lsb
           shrc                        ; and shift it
           str     r8                  ; put it back
           dec     rd                  ; decrement shift count
           lbr     tkn_shrlp           ; loop back for more shifts

; ************************************
; *** shl 2 tokens                 ***
; *** R8 - address of token to add ***
; *** Returns: DF=1 error          ***
; ************************************
tkn_shl:   sep     scall               ; setup arguments
           dw      tkn_arg
           lbdf    tkn_error           ; jump if arguments invalid
tkn_shllp: dec     r8                  ; point r8 at msb
           glo     rd                  ; see if more shifts needed
           lbz     del_2               ; jump if done
           inc     r8                  ; point to lsb
           ldn     r8                  ; get .sb
           shl                         ; and shift it
           str     r8                  ; put it back
           dec     r8                  ; point to msb
           ldn     r8                  ; get msb
           shlc                        ; and shift it
           str     r8                  ; put it back
           inc     r8                  ; back to lsb
           dec     rd                  ; decrement shift count
           lbr     tkn_shllp           ; loop back for more shifts

; **************************
; *** process . operator ***
; **************************
tkn_dot:   sep     scall               ; setup arguments
           dw      tkn_arg
           lbdf    tkn_error           ; jump if invalid arguments
           ghi     rd                  ; high byte must be zero
           lbnz    tkn_error           ; else error
           glo     rd                  ; low must be zero or 1
           ani     0feh
           lbnz    tkn_error           ; else error
           glo     rd                  ; see what we need
           lbz     tkn_dotlo           ; jump for low
           dec     r8                  ; point to msb
           lda     r8                  ; retrieve value
           str     r8                  ; and store into lo byte
tkn_dotlo: dec     r8                  ; back to msb
           ldi     0                   ; needs to be zero
           str     r8                  ; store it
           lbz     del_2               ; clean up

; *****************************
; *** Process HIGH operator ***
; *****************************
tkn_high:  ldi     3                   ; next token is 3 bytes away
           sep     scall               ; point to next token
           dw      tkn_get_2
           smi     1                   ; must be numeric
           lbnz    tkn_error           ; else error
           ghi     rd                  ; get high value
           plo     rd                  ; and set for write
tkn_nyb:   ldi     1                   ; convert token to numeric
           str     r8
           inc     r8                  ; point to msb of token
           ldi     0                   ; and store a zero
           str     r8
           inc     r8
           glo     rd                  ; get value
           str     r8                  ; and store
           inc     r8                  ; token to delete
           sep     scall               ; delete token
           dw      tkn_del
           dec     r8                  ; point back to numeric token
           dec     r8
           dec     r8
           sep     sret                ; and return

; ****************************
; *** Process LOW operator ***
; ****************************
tkn_low:   ldi     3                   ; next token is 3 bytes away
           sep     scall               ; point to next token
           dw      tkn_get_2
           smi     1                   ; must be numeric
           lbnz    tkn_error           ; else error
           lbr     tkn_nyb             ; save the value

; ***********************
; *** Process ( token ***
; ***********************
tkn_op:    ldi     high pstart         ; point to storage
           phi     rd
           ldi     low pstart
           plo     rd
           ghi     r8                  ; store token address
           str     rd
           inc     rd
           glo     r8
           str     rd
           adi     0                   ; signify no error
           inc     r8                  ; move past token
           inc     r8
           inc     r8
           sep     sret                ; and return

; ***********************
; *** Process ) token ***
; ***********************
tkn_cp:    ldi     high pstart         ; point to start token
           phi     rd
           ldi     low pstart
           plo     rd
           ldn     rd                  ; get byte
           smi     0ffh                ; check for invalid token
           lbz     tkn_cperr           ; jump if no open parens
           glo     rb                  ; save beginning of stream
           stxd
           ghi     rb
           stxd
           sep     scall               ; delete the close parens
           dw      tkn_del
           glo     r8                  ; transfer current token to R9
           plo     r9
           ghi     r8
           phi     r9
           lda     rd                  ; get open parens addr
           phi     r8
           lda     rd
           plo     r8
           sep     scall               ; delete the open parens
           dw      tkn_del
           dec     r9                  ; account for deleted open paren
           dec     r9
           dec     r9
           ldn     r9                  ; get token type
           stxd                        ; save it
           ldi     0                   ; need a zero marker
           str     r9                  ; store into close paren token
           sep     scall               ; count tokens inside parens
           dw      tkn_count
           irx                         ; recover token type
           ldx
           str     r9
           glo     r7                  ; save R7
           stxd
           ghi     r7
           stxd
           sep     scall               ; reduce expr in the parens
           dw      reduce
           irx                         ; recover original R8 and RB, and R7
           ldxa
           phi     r7
           ldxa
           plo     r7
           ldxa
           phi     rb
           phi     r8                  ; reset stream pointer
           ldx
           plo     rb
           plo     r8
           lbdf    tkn_cperr           ; jump on eval error
           sep     scall               ; get new token count
           dw      tkn_count
           ldi     high pstart         ; need to signal no open parens
           phi     rd
           ldi     low pstart
           plo     rd
           ldi     0ffh                ; signal for no open parens
           str     rd
           phi     r9                  ; no end pointer
           plo     r9
           adi     0                   ; signal no error
           sep     sret                ; return to caller
tkn_cperr: smi     0                   ; signal an error
           sep     sret                ; and return

; ***************************************
; *** Check for end of token sequence ***
; *** R8 - current token              ***
; *** R9 - end token                  ***
; *** Returns: DF=1 if at end         ***
; ***************************************
checkend:  ldn     r8                  ; see if pointing at null token
           lbz     atend               ; jump if at end
           glo     r9                  ; compare token addresses
           str     r2
           glo     r8
           sm
           lbnz    notend              ; jump if not equal
           ghi     r9                  ; check high byte
           str     r2
           ghi     r8
           sm
           lbnz    notend              ; jump if not equal
atend:     smi     0                   ; signal at end
           sep     sret                ; and return
notend:    adi     0                   ; signal not at end
           sep     sret                ; and return

; **************************************
; *** Count tokens                   ***
; *** R8 - beginning of token string ***
; *** Returns: RC - token count      ***
; **************************************
tkn_count: ldi     0                   ; setup count 
           plo     rc
           glo     r8                  ; get address
           plo     rd
           ghi     r8
           phi     rd
tkn_ctlp:  lda     rd                  ; get byte from token
           lbz     tkn_ctdn            ; jump if at end
           inc     rd                  ; move to next token
           inc     rd
           inc     rc                  ; increment token count
           lbr     tkn_ctlp            ; keep counting
tkn_ctdn:  sep     sret                ; return to caller

; ***********************************
; *** Process group               ***
; *** R8 - Starting token address ***
; *** R9 - Ending token address   ***
; *** R7 - allowed codes          ***
; *** Returns: DF=1 error         ***
; ***********************************
prc_grp2:  inc     r8                  ; move to next token
           inc     r8
prc_grp:   sep     scall               ; see if at end of stream
           dw      checkend
           lbdf    prc_grpdn           ; jump if done
           lda     r8                  ; get token type
           smi     2                   ; must be 2 for function
           lbnz    prc_grp2            ; jump if not
           inc     r8                  ; point to lsb of token value
           lda     r8                  ; get token lsb
           str     r2                  ; place into memory
           glo     r7                  ; check against first coce
           sm
           lbz     prc_grpys           ; jump on match
           ghi     r7                  ; check against high code
           sm
           lbnz    prc_grp             ; jump if no go
prc_grpys: dec     r8                  ; move pointer back to beginnig of token
           dec     r8
           dec     r8
           ldn     r2                  ; get function number
           shl                         ; multiply by 2
           str     r2                  ; put back in memory
           ldi     low jumptab         ; point to entry in jump table
           add                         ; add in offset
           plo     rd                  ; put into pointer
           ldi     high jumptab
           adci    0                   ; propagate carry
           phi     rd                  ; rd now has address of routine
           ldi     high calladdr       ; get call address
           phi     rf
           ldi     low calladdr
           plo     rf
           lda     rd                  ; transfer address
           str     rf
           inc     rf
           ldn     rd
           str     rf
           sep     scall               ; call function
calladdr:  dw      0
           lbdf    prc_err             ; jump if error occurred
           lbr     prc_grp             ; and continue processing
prc_grpdn: ghi     rb                  ; reset pointer
           phi     r8
           glo     rb
           plo     r8 
           adi     0                   ; no error when nothing processed
           sep     sret                ; and return to caller
prc_err:   smi     0                   ; signal an error
           sep     sret                ; and return

; ***********************************
; *** Evaluate an expression      ***
; *** R8 - Starting token address ***
; *** R9 - Ending token address   ***
; *** RC - Token Count            ***
; *** Returns: RD - value         ***
; ***          DF=1 error         ***
; ***********************************
reduce:    ldn     r8                  ; is there an expression to evaluate
           lbz     reduceno            ; jump if not
           ghi     r8                  ; save beginning of token sequence
           phi     rb
           glo     r8
           plo     rb
; ***********************
; *** Look for parens ***
; ***********************
           ldi     0                   ; code for open paren
           plo     r7                  ; setup for call
           ldi     1                   ; code for close paren
           phi     r7
           ldi     high pstart         ; point to parens start
           phi     rd
           ldi     low pstart
           plo     rd
           ldi     0ffh                ; signal no open parens
           str     rd
           sep     scall               ; process the group
           dw      prc_grp
           lbdf    red_err             ; jump if error occured
; ******************************
; *** Look for dot operators ***
; ******************************
           ldi     2                   ; code for dot operator
           plo     r7                  ; setup for call
           ldi     0ffh                ; no high code
           phi     r7
           sep     scall               ; process the group
           dw      prc_grp
           lbdf    red_err             ; jump if error occured
; **********************************
; *** Look for * and / operators ***
; **********************************
           ldi     3                   ; code for * operator
           plo     r7                  ; setup for call
           ldi     4                   ; code for / operator
           phi     r7
           sep     scall               ; process the group
           dw      prc_grp
           lbdf    red_err             ; jump if error occured
; ****************************
; *** Look for % operators ***
; ****************************
           ldi     5                   ; code for dot operator
           plo     r7                  ; setup for call
           ldi     0ffh                ; no high code
           phi     r7
           sep     scall               ; process the group
           dw      prc_grp
           lbdf    red_err             ; jump if error occured
; **********************************
; *** Look for + and - operators ***
; **********************************
           ldi     6                   ; code for + operator
           plo     r7                  ; setup for call
           ldi     7                   ; code for - operator
           phi     r7
           sep     scall               ; process the group
           dw      prc_grp
           lbdf    red_err             ; jump if error occured
; ****************************
; *** Look for & operators ***
; ****************************
           ldi     8                   ; code for & operator
           plo     r7                  ; setup for call
           ldi     0ffh                ; no high code
           phi     r7
           sep     scall               ; process the group
           dw      prc_grp
           lbdf    red_err             ; jump if error occured
; **********************************
; *** Look for | and ^ operators ***
; **********************************
           ldi     9                   ; code for | operator
           plo     r7                  ; setup for call
           ldi     10                  ; code for ^ operator
           phi     r7
           sep     scall               ; process the group
           dw      prc_grp
           lbdf    red_err             ; jump if error occured
; **************************************
; *** Look for shl and shr operators ***
; **************************************
           ldi     11                  ; code for shr operator
           plo     r7                  ; setup for call
           ldi     12                  ; code for shl operator
           phi     r7
           sep     scall               ; process the group
           dw      prc_grp
           lbdf    red_err             ; jump if error occured
; ***************************************
; *** Look for high and low operators ***
; ***************************************
           ldi     13                  ; code for high operator
           plo     r7                  ; setup for call
           ldi     14                  ; code for low operator
           phi     r7
           sep     scall               ; process the group
           dw      prc_grp
           lbdf    red_err             ; jump if error occured
; *********************************************
; *** Check if expression was fully reduced ***
; *********************************************
           glo     rc                  ; get token count
           smi     1                   ; must be 1
           lbnz    red_err             ; otherwise error
           lda     r8                  ; get token type
           smi     1                   ; must be numeric
           lbnz    red_err             ; otherwise error
           lda     r8                  ; retrieve value
           phi     rd
           lda     r8 
           plo     rd
           adi     0                   ; signal success
           sep     sret                ; and return
red_err:   smi     0                   ; signal an error
           sep     sret                ; and return to caller
reduceno:  ldi     0                   ; set value as zero
           phi     rd
           plo     rd
           adi     0                   ; indicate no error
           sep     sret                ; and return

jumptab:   dw      tkn_op
           dw      tkn_cp
           dw      tkn_dot
           dw      tkn_mul
           dw      tkn_div
           dw      tkn_mod
           dw      tkn_add
           dw      tkn_sub
           dw      tkn_and
           dw      tkn_or
           dw      tkn_xor
           dw      tkn_shr
           dw      tkn_shl
           dw      tkn_high
           dw      tkn_low
         
functab:   db      ('('+80h)             ; 0
           db      (')'+80h)             ; 1
           db      ('.'+80h)             ; 2
           db      ('*'+80h)             ; 3
           db      ('/'+80h)             ; 4
           db      ('%'+80h)             ; 5
           db      ('+'+80h)             ; 6
           db      ('-'+80h)             ; 7
           db      ('&'+80h)             ; 8
           db      ('|'+80h)             ; 9
           db      ('^'+80h)             ; 10
           db      'SH',('R'+80h)        ; 11
           db      'SH',('L'+80h)        ; 12
           db      'HIG',('H'+80h)       ; 13
           db      'LO',('W'+80h)        ; 14
           db      0

; ***********************************
; *** End of expression evaluator ***
; ***********************************

; *************************************
; *** Check if symbol is a function ***
; *** RF - pointer to symbol        ***
; *** Returns: RD - function number ***
; ***          DF=1 - is function   ***
; ***          DF=0 - not function  ***
; *************************************
tokenfunc: ldi     high functab        ; point to function table
           phi     r7
           ldi     low functab
           plo     r7
           ldi     0                   ; setup function number
           plo     rd
           ghi     rf                  ; save beginning
           phi     rb
           glo     rf
           plo     rb
tfloop:    ldn     r7                  ; see if at last token
           lbnz    tfgo                ; jump if not
           adi     0                   ; signify not a function
           sep     sret                ; and return to caller
tfgo:      ldn     r7                  ; get token byte
           ani     080h                ; see if last one
           lbnz    tflast              ; jump if it is
           lda     r7                  ; get byte from function name
           str     r2                  ; store for compare
           lda     rf                  ; get byte from buffer
           sm                          ; and compare
           lbz     tfgo                ; jump if they matched
tfnolp:    lda     r7                  ; get byte from function table
           ani     080h                ; check for last character
           lbz     tfnolp              ; jump if not it
tfno:      inc     rd                  ; increment function number
           ghi     rb                  ; restore buffer pointer
           phi     rf
           glo     rb
           plo     rf
           lbr     tfloop              ; and check next function
tflast:    lda     r7                  ; get byte
           ani     07fh                ; strip off high bit
           str     r2                  ; prepare for compare
           lda     rf                  ; get byte from buffer
           sm                          ; see if they match
           lbnz    tfno                ; jump if not
           smi     0                   ; signal function found
           sep     sret                ; and return


; **************************************
; *** Expression evaluator tokenizer ***
; *** RF - Ascii string to tokenize  ***
; *** Returns: RF - char after expr  ***
; ***          R8 - first token      ***
; ***          R9 - last token       ***
; ***          RC - token count      ***
; ***          DF=1 - error          ***
; **************************************
tokenize:  ldi     high tokens         ; point to token storage
           phi     r9
           ldi     low tokens
           plo     r9
           ldi     0                   ; set token count
           plo     rc
tokenlp:   sep     scall               ; move past any whitespace
           dw      ltrim
           ldn     rf                  ; check for line terminator
           lbz     tokendn             ; jump if terminator
           smi     ','                 ; check for comma
           lbz     tokendn             ; this also terminates the sequence
           ldn     rf                  ; check character constant
           smi     27h
           lbz     charstr
           ldn     rf                  ; recover byte
           sep     scall               ; see if it is a number
           dw      identify            ; identify string
           lbz     tokennum            ; jump if number
           smi     1                   ; check if it was hex number
           lbz     tokennum            ; jump if so
           sep     scall               ; see if it is a function
           dw      tokenfunc
           lbdf    isfunc              ; jump if it is
           ldn     rf                  ; get byte
           smi     'R'                 ; check possible register name
           lbz     tokenreg            ; could be, so check
           smi     32                  ; check lowercase too
           lbz     tokenreg            ; could be, so check
token2:    glo     r8                  ; getsymbol destroys r8
           stxd
           ghi     r8
           stxd
           sep     scall               ; must be a symbol at this point
           dw      getsymbol
           irx                         ; recover r8
           ldxa
           phi     r8
           ldx
           plo     r8
           lbdf    tokenerr            ; jump if not found
           lbr     tokennum            ; process value
tokenreg:  inc     rf                  ; point to next character
           ldn     rf                  ; get it
           sep     scall               ; see if it is a hex character
           dw      ishex
           lbdf    tokenreg2           ; maybe, need to check next char
           dec     rf                  ; nope, so move back
           lbr     token2
tokenreg2: inc     rf                  ; point to next character
           ldn     rf                  ; get it
           sep     scall               ; check for valid lable character
           dw      isalnum
           lbnf    tokenreg3           ; looks good
           dec     rf                  ; nope, so move back to beginning
           dec     rf
           lbr     token2              ; process for label
tokenreg3: dec     rf                  ; move back to register number
           lda     rf                  ; get byte
           sep     scall               ; convert to binary
           dw      hexbin
           plo     rd                  ; setup for number
           ldi     0                   ; high value is zero
           phi     rd
           lbr     tokennum            ; now process as a number
isfunc:    ldi     2                   ; specify a function
           lbr     tokennmgo           ; and write token
tokennum:  ldi     1                   ; token is a number
tokennmgo: str     r9                  ; write to token
           inc     r9
           ghi     rd                  ; write value to token
           str     r9
           inc     r9
           glo     rd                  ; write lo value to token
           str     r9
           inc     r9
           inc     rc                  ; increment token count
           lbr     tokenlp             ; loop back for more tokens
tokendn:   ldi     0                   ; need to terminate token sequence
           str     r9
           ldi     high tokens         ; set start token
           phi     r8
           ldi     low tokens
           plo     r8
           adi     0                   ; signify no error
           sep     sret                ; and return
tokenerr:  smi     0                   ; signal an error
           sep     sret                ; and return
charstr:   inc     rf                  ; move to next char
           lda     rf                  ; retrieve constant
           plo     rd                  ; setup as a number
           ldi     0
           phi     rd
charstrlp: lda     rf                  ; need to find close
           lbz     tokenerr            ; must not find a terminator
           smi     27h                 ; look for close quote
           lbnz    charstrlp           ; keep looking
           lbr     tokennum            ; process as a number

; **********************************************************
; ***** Convert string to uppercase, honor quoted text *****
; **********************************************************
touc:      ldn     rf                  ; check for quote
           smi     027h
           lbz     touc_qt             ; jump if quote
           ldn     rf                  ; get byte from string
           lbz     touc_dn             ; jump if done
           smi     'a'                 ; check if below lc
           lbnf    touc_nxt            ; jump if so
           smi     27                  ; check upper rage
           lbdf    touc_nxt            ; jump if above lc
           ldn     rf                  ; otherwise convert character to lc
           smi     32
           str     rf
touc_nxt:  inc     rf                  ; point to next character
           lbr     touc                ; loop to check rest of string
touc_dn:   sep     sret                ; return to caller
touc_qt:   inc     rf                  ; move past quote
touc_qlp:  lda     rf                  ; get next character
           lbz     touc_dn             ; exit if terminator found
           smi     027h                ; check for quote charater
           lbz     touc                ; back to main loop if quote
           lbr     touc_qlp            ; otherwise keep looking

pstart:    dw      0
char:      db      0
fname:     dw      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
fname2:    dw      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
fildes:    db      0,0,0,0,050h,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

fileerr:   db      'File Error',10,13,0
errmsg:    db      'Error'
crlf:      db      10,13,0
nxtline:   db      10,13,'     ',0

lastsym:   dw      0
pass:      db      0
bytecnt:   db      0
curaddr:   dw      0
linenum:   dw      0

lowmem:    dw      0ffffh
highmem:   dw      0
startaddr: dw      0
insttab:   db      'AD',('D'+80h),1,0f4h,0
           db      'AD',('C'+80h),1,074h,0
           db      'ADC',('I'+80h),2,07ch,0
           db      'AD',('I'+80h),2,0fch,0
           db      'AN',('D'+80h),1,0f2h,0
           db      'AN',('I'+80h),2,0fah,0
           db      'B',('1'+80h),2,034h,0
           db      'B',('2'+80h),2,035h,0
           db      'B',('3'+80h),2,036h,0
           db      'B',('4'+80h),2,037h,0
           db      'BD',('F'+80h),2,033h,0
           db      'BN',('1'+80h),2,03ch,0
           db      'BN',('2'+80h),2,03dh,0
           db      'BN',('3'+80h),2,03eh,0
           db      'BN',('4'+80h),2,03fh,0
           db      'BN',('F'+80h),2,03bh,0
           db      'BN',('Q'+80h),2,039h,0
           db      'B',('Q'+80h),2,031h,0
           db      'B',('R'+80h),2,030h,0
           db      'B',('Z'+80h),2,032h,0
           db      'BN',('Z'+80h),2,03ah,0
           db      'DE',('C'+80h),5,020h,0
           db      'DI',('S'+80h),1,071h,0
           db      'GH',('I'+80h),5,090h,0
           db      'GL',('O'+80h),5,080h,0
           db      'ID',('L'+80h),1,000h,0
           db      'IN',('C'+80h),5,010h,0
           db      'IN',('P'+80h),4
           dw           opinp
           db      'IR',('X'+80h),1,060h,0
           db      'LBD',('F'+80h),3,0c3h,0
           db      'LBN',('F'+80h),3,0cbh,0
           db      'LBN',('Q'+80h),3,0c9h,0
           db      'LBN',('Z'+80h),3,0cah,0
           db      'LB',('Q'+80h),3,0c1h,0
           db      'LB',('R'+80h),3,0c0h,0
           db      'LD',('A'+80h),5,040h,0
           db      'LD',('I'+80h),2,0f8h,0
           db      'LD',('N'+80h),4
           dw           opldn
           db      'LD',('X'+80h),1,0f0h,0
           db      'LDX',('A'+80h),1,072h,0
           db      'LSD',('F'+80h),1,0cfh,0
           db      'LSI',('E'+80h),1,0cch,0
           db      'LSN',('F'+80h),1,0c7h,0
           db      'LSN',('Q'+80h),1,0c5h,0
           db      'LSN',('Z'+80h),1,0c6h,0
           db      'LS',('Q'+80h),1,0cdh,0
           db      'LS',('Z'+80h),1,0ceh,0
           db      'MAR',('K'+80h),1,079h,0
           db      'NB',('R'+80h),1,038h,0
           db      'NLB',('R'+80h),1,0c2h,0
           db      'NO',('P'+80h),1,0c4h,0
           db      'O',('R'+80h),1,0f1h,0
           db      'OR',('I'+80h),2,0f9h,0
           db      'OU',('T'+80h),4
           dw           opout
           db      'PH',('I'+80h),5,0b0h,0
           db      'PL',('O'+80h),5,0a0h,0
           db      'RE',('T'+80h),1,070h,0
           db      'RE',('Q'+80h),1,07ah,0
           db      'S',('D'+80h),1,0f5h,0
           db      'SA',('V'+80h),1,078h,0
           db      'SD',('B'+80h),1,075h,0
           db      'SDB',('I'+80h),2,07dh,0
           db      'SD',('I'+80h),2,0fdh,0
           db      'SE',('P'+80h),5,0d0h,0
           db      'SE',('Q'+80h),1,07bh,0
           db      'SE',('X'+80h),5,0e0h,0
           db      'SH',('L'+80h),1,0feh,0
           db      'SH',('R'+80h),1,0f6h,0
           db      'SHL',('C'+80h),1,07eh,0
           db      'SHR',('C'+80h),1,076h,0
           db      'S',('M'+80h),1,0f7h,0
           db      'SM',('B'+80h),1,077h,0
           db      'SMB',('I'+80h),2,07fh,0
           db      'SM',('I'+80h),2,0ffh,0
           db      'ST',('R'+80h),5,050h,0
           db      'STX',('D'+80h),1,073h,0
           db      'XO',('R'+80h),1,0f3h,0
           db      'XR',('I'+80h),2,0fbh,0
           db      'EN',('D'+80h),4
           dw           opend
           db      'OR',('G'+80h),4
           dw           oporg
           db      'D',('B'+80h),4
           dw           opdb
           db      'D',('W'+80h),4
           dw           opdw
           db      'D',('S'+80h),4
           dw           opds
           db      0

endrom:    equ     $

outbuffer: ds      80

buffer:    ds      80

tokens:    ds      32*3

symtab:    db      0

