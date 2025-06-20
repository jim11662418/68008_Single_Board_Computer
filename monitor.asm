         page 0                     ; suppress page headings in .lst file
         cpu 68000
         supmode ON                 ; suppress warnings about privileded instructions
         include "bitfuncs.inc"

; A very basic monitor for my 68008 single board computer.
;
; derived from https://github.com/douggilliland/Linux-68k/blob/master/SIMPLE_68008/Monitor-Simple/Monitor-Simple.x68
;
; Assemble with the Macro Assembler AS V1.42 http://john.ccac.rwth-aachen.de:8000/as/
;
; Monitor Commands:
;  (B)ASIC
;     run Lee Davidson's Enhanced BASIC
;  (D)eposit
;     D1000 AA 55 -- write $AA to address $1000 and $55 to $1001
;  (E)xamine memory
;     E100-104 -- dump memory from $100-$103
;  (F)ill memory
;     F 1000 AA 100 -- fill memory $1000-$10FF with $AA
;  (H)elp
;     display these commands
;  (R)un
;     R2000 -- run code starting at address $2000
;  (S)REC
;     load SREC record
;  (U)ptime
;     print uptime as HH:MM:SS
;
; Serial I/O 38400 bps, N-8-1, RTS/CTS handshaking
;
; Memory Map:
;  $000000-$07FFFF RAM
;  $080000-$09FFFF EPROM
;  $0E0000         LED port
;  $0F0000         DUART

STACK          = $07FFFC               ; Has to be on a word boundary
ROM_START      = $080000               ; ROM start
LEDs           = $0E0000               ; LED port
DUART          = $0F0000               ; base address of DUART
Vector         = $000100               ; DUART Timer interrupt vector address in SRAM

flashCount     = 8192                  ; number of times thru loop for flashing orange LED
MAX_LINE_LEN   = 80                    ; input buffer max

; 68681 DUART register addresses
MRA            = DUART+0               ; Mode Register A             (R/W)
SRA            = DUART+2               ; Status Register A           (R)
CSRA           = DUART+2               ; Clock Select Register A     (W)
CRA            = DUART+4               ; Commands Register A         (W)
RBA            = DUART+6               ; Receiver Buffer A           R)
TBA            = DUART+6               ; Transmitter Buffer A        (W)
ACR            = DUART+8               ; Aux. Control Register       (R/W)
ISR            = DUART+10              ; Interrupt Status Register   (R)
IMR            = DUART+10              ; Interrupt Mask Register     (W)
CUR            = DUART+12              ; Current MSB of Counter      (R)
CTUR           = DUART+12              ; Counter/Timer Upper Register(W)
CUL            = DUART+14              ; Current LSB of Counter      (R)
CTLR           = DUART+14              ; Counter/Timer Lower Register(W)
MRB            = DUART+16              ; Mode Register B             (R/W)
SRB            = DUART+18              ; Status Register B           (R)
CSRB           = DUART+18              ; Clock Select Register B     (W)
CRB            = DUART+20              ; Commands Register B         (W)
RBB            = DUART+22              ; Reciever Buffer B           (R)
TBB            = DUART+22              ; Transmitter Buffer B        (W)
IVR            = DUART+24              ; Interrupt Vector Register   (R/W)
OPC            = DUART+26              ; Output port config          (W)
INU            = DUART+26              ; Input port (unlatched)      (R)
OPS            = DUART+28              ; Output port Set             (W)
OPR            = DUART+30              ; Output port Clear           (W)

; ASCII control characters
BKSP           = $08                   ; CTRL-H
TAB            = $09
LF             = $0A
CR             = $0D
ESC            = $1B

CTRLC          = $03
CTRLX          = $18                   ; Line Clear

; VT100 attributes escape sequences
SGR0           = "\x1B[0m"             ; turn off character attributes
SGR1           = "\x1B[1m"             ; turn bold mode on
SGR2           = "\x1B[2m"             ; turn low intensity mode on
SGR4           = "\x1B[4m"             ; turn underline mode on
SGR5           = "\x1B[5m"             ; turn blinking mode on
SGR7           = "\x1B[7m"             ; turn reverse video on

; SRAM usage
               ORG $07FB00
RxBuffer:      ds.b 256
inPtr:         ds.l 1                  ; Rx buffer input pointer
outPtr:        ds.l 1                  ; Rx buffer output pointer
LEDcounter     ds.w 1                  ; counter for flashing orange LED
ticks          ds.b 1                  ; ticks counter
secs           ds.b 1                  ; uptime seconds counter
mins           ds.b 1                  ; uptime minutes counter
hrs            ds.b 1                  ; uptime hours counter
flags          ds.b 1
lineBuffPtr    ds.l 1
lineBuffer     ds.b MAX_LINE_LEN

               ORG ROM_START
               dc.l     STACK          ; Reset - initial supervisor stack pointer
               dc.l     reset          ; Reset - initial program counter value

; power-on-reset starts here
reset:         nop
               lea      STACK,SP
               ; write to address 0x80000 with D0=1 to unmap the EPROM from the SRAM space.
               ; needs to be done early in the startup code before SRAM is accessed.
               move.b   #$FF,$080000
               move.b	#$80,OPS		   ; turn off orange LED
               move.b   #$00,LEDs      ; turn off yellow LEDs
               clr.b    ticks          ; clear the ticks counter
               clr.b    secs           ; clear the seconds counter
               clr.b    mins           ; clear the minutes counter
               clr.b    hrs            ; clear the hours counter
               clr.w    LEDcounter     ; clear counter for flashing orange LED
               move.b   #$80,flags     ; set blinking LED flag

               ; fill in the vector table $000000-$0000FC in SRAM
               lea      $000000,A0     ; start of vector table in SRAM
               move.l   #STACK,(A0)+   ; initial SP value (even though a reset will actually start from EPROM)
               move.l   #reset,(A0)+   ; initial PC value (even though a reset will actually start from EPROM)
reset1:        move.l   #error,(A0)+   ; fill in the 62 vector table entries $000008-$0000FC with the address of 'error'
               cmp.l    #$000100,A0
               bne      reset1

               clr.l    inPtr          ; initialize Rx buffer pointers
               clr.l    outPtr

               ; Initialize the 68681 DUART port A as 38400 bps, N-8-1, no RTS/CTS handshaking.
               move.b   #$30,CRA       ; reset Transmitter A
               move.b   #$20,CRA       ; reset Receiver A
               move.b   #$10,CRA       ; reset Mode Register Pointer A
               move.b   #$00,ACR       ; baud rate set #2
               move.b   #$CC,CSRA      ; set Tx and Rx rates to 38400
               move.b   #$13,MRA       ; MR1A - Rx RTS control cisabled, no Parity, 8-bit
               move.b   #$07,MRA       ; MR2A - Normal Mode, disable TxRTS and TxCTS, 1 stop bit
               move.b   #$05,CRA       ; enable Transmit/Recieve
               move.b	#$00,OPC		   ; output port configuration (all bits are outputs)
               move.b	#$FF,OPR		   ; clear all outputs

               ; set up the DUART Timer to interrupt every 16.66 mSec (60 times/second)
               move.b   ACR,D0         ; read ACR
               andi.b   #$8F,D0        ; mask ACR bits
               ori.b    #$70,D0        ; Timer mode using XTAL (3.6864 MHz) divided by 16
               move.b   D0,ACR         ; write back ACR
               ; set the timer period
               ; timer period = 2*(preload)*(clock period)
               ;  0.010   Sec = 2*(1152)   *(1/(3686400/16))
               ;  0.01666 Sec = 2*(1920)   *(1/(3686400/16))
               ;  0.020   Sec = 2*(2304)   *(1/(3686400/16))
               ;  0.050   Sec = 2*(5760)   *(1/(3686400/16))
               move.b   #hi(1920),CTUR ; Counter/Timer Upper Register
               move.b   #lo(1920),CTLR ; Counter/Timer Lower Register
               move.b   OPS,D0         ; read of OPS issues Start Counter command

               ori.w	   #$0700,SR		; disable interrupts
               move.l	#RxISR,Vector  ; interrupt vector table entry for DUART interrupt service routine
               move.b	#Vector/4,IVR  ; Interrupt Vector Register

               ; set DUART interrupt mask to enable Counter/Timer and channel A Rx interrupts
               move.b   #$0A,IMR       ; Interrupt Mask Register
               andi.w   #$F8FF,SR      ; enable interrupts

               lea      msgBanner,A0
               jsr      puts           ; print the initial message
               lea      msgHelp,A0
               jsr      puts

; monitor command interpreter loop
interpreter:   lea      msgPrompt,A0   
               bsr      puts           ; print the prompt
               jsr      readLine       ; get a line from the console
               jsr      lineToUpper    ; convert input line to upper case
               jsr      parseLine      ; parse line for commands
               bra      interpreter    ; loop back for another line

;----------------------------------------------------------------------------
; read a line from the console into the line buffer
;----------------------------------------------------------------------------
readLine:      movem.l  D2/A2,-(SP)    ; save changed registers
               lea      lineBuffer,A2  ; start of the lineBuffer
               eor.w    D2,D2          ; clear the character counter
readLine1:     bsr      getc           ; read a character from the serial port
               cmp.b    #BKSP,D0       ; is it a backspace?
               beq.s    readLine2
               cmp.b    #CTRLX,D0      ; is it ^X (Line Clear)?
               beq      readLine3
               cmp.b    #CR,D0         ; is it a carriage return?
               beq      readLine5
               cmp.b    #LF,D0         ; is it anything else but a LF?
               beq      readLine1      ; Ignore LFs and get the next character
               ; normal character to be inserted into the buffer
               cmp.w    #MAX_LINE_LEN,D2
               bge      readLine1      ; if the buffer is full ignore the character
               move.b   D0,(A2)+       ; otherwise store the character
               addq.w   #1,D2          ; increment character count
               bsr      putc           ; echo the character
               bra      readLine1      ; and get the next one

readLine2:     tst.w    D2             ; are we at the beginning of the line?
               beq      readLine1      ; then ignore it
               bsr      putc           ; backspace
               move.b   #' ',D0
               bsr      putc           ; space
               move.b   #BKSP,D0
               bsr      putc           ; backspace
               subq.l   #1,A2          ; move back in the buffer
               subq.l   #1,D2          ; and current character count
               bra      readLine1      ; and goto the next character

readLine3:     tst      D2             ; anything to clear?
               beq      readLine1      ; if not,fetch the next character
               suba.l   D2,A2          ; return to the start of the buffer
readLine4:     move.b   #BKSP,D0
               bsr      putc           ; backspace
               move.b   #' ',D0
               bsr      putc           ; space
               move.b   #BKSP,D0
               bsr      putc           ; backspace
               subq.w   #1,D2
               bne      readLine4      ; go till the start of the line
               bra      readLine1

readLine5:     bsr      putc           ; echo the CR
               move.b   #LF,D0
               bsr      putc           ; line feed to be safe
               clr.b    (A2)           ; terminate the line (buffer is longer than max to allow this at full length)
               movea.l  A2,A0          ; ready the pointer to return (if needed)
               movem.l  (SP)+,D2/A2    ; restore registers
               rts                     ; and return

;----------------------------------------------------------------------------
; convert the text in the line buffer to upper case
;----------------------------------------------------------------------------
lineToUpper:   lea      lineBuffer,A0  ; get the start of the line buffer
lineToUpper1:  move.b   (A0),D0        ; read in a character
               cmp.b    #'a',D0
               blt      lineToUpper2   ; is it less than lower-case 'a', then move on
               cmp.b    #'z',D0
               bgt      lineToUpper2   ; is it greater than lower-case 'z', then move on
               sub.b    #$20,D0        ; then convert a to A, b to B, etc.
lineToUpper2:  move.b   D0,(A0)+       ; store the character back into A0, and move to the next
               bne      lineToUpper1   ; keep going till we hit a null terminator
               rts

;----------------------------------------------------------------------------
; parse the line buffer for commands
;----------------------------------------------------------------------------
parseLine:     movem.l  D0/A0,-(SP)    ; save registers
               lea      lineBuffer,A0
parseLine1:    move.b   (A0)+,D0
               cmp.b    #' ',D0        ; ignore spaces
               beq      parseLine1
               cmp.b    #'B',D0        ; BASIC
               beq      runBASIC
               cmp.b    #'D',D0        ; Deposit command
               beq      depositCMD
               cmp.b    #'E',D0        ; Examine command
               beq      examineCMD
               cmp.b    #'F',D0        ; Fill
               beq      fillCMD
               cmp.b    #'H',D0        ; Help command
               beq      helpCMD
               cmp.b    #'R',D0        ; Run command
               beq      runCMD
               cmp.b    #'S',D0        ; SREC record command
               beq      loadSRec
               cmp.b    #'U',D0        ; Uptime
               beq      uptimeCMD
               cmp.b    #0,D0          ; ignore blank lines
               beq      parseLineExit
invalidCMD:    lea      msgInvalidCMD,A0
               bsr      puts
parseLineExit: movem.l  (SP)+,D0/A0    ; restore registers
               rts

invalidAddr:   lea      msgInvalidAddr,A0
               bsr      puts
               bra      parseLineExit

invalidVal:    lea      msgInvalidVal,A0
               bsr      puts
               bra      parseLineExit

;============================================================================
; Load Motorola S Records
;
; supports 16 bit S1 record types...
;  S0030000FC
;  S5030010EC
;  S1131000000102030405060708090A0B0C0D0E0F64
;  S9030000FC;
;
; also supports 24 bit S2 record types...
;  S0030000FC
;  S5030010EC
;  S214010000000102030405060708090A0B0C0D0E0F72
;  S804000000FB
;
; and also 32 bit S3 record types...
;  S0030000FC
;  S5030010EC
;  S31510000000000102030405060708090A0B0C0D0E0F62
;  S70500000000FA
;
; at present, S0 and S5 record types are ignored.
;
; D0 holds the data from the input buffer
; D1 is used by the 'get2hex' function
; D2 holds the record's type
; D3 holds the record's byte count
; D4 holds the record's computed checksum
; A3 holds the record's address
;============================================================================
loadSRec:      clr.l    D4             ; clear the record's computed checksum
               clr.l    D3             ; clear the record's byte count

; get the record's type
               move.b   (A0)+,D2       ; get the next character (record type) from the buffer
               cmp.b    #'0',D2        ; header type record?
               beq      loadSRec99     ; ignore header type records (for now)
               cmp.b    #'1',D2        ; 16 bit address data record?
               beq      loadSRec1      ; go get the data record's byte count
               cmp.b    #'2',D2        ; 24 bit address data record?
               beq      loadSRec1      ; go get the data record's byte count
               cmp.b    #'3',D2        ; 32 bit address data record?
               beq      loadSRec1      ; go get the data record's byte count
               cmp.b    #'5',D2        ; count record?
               beq      loadSRec99     ; ignore count records (for now)
               cmp.b    #'7',D2        ; 32 bit start address record?
               beq      loadSRec1      ; go get the start record's byte count
               cmp.b    #'8',D2        ; 24 bit start address record?
               beq      loadSRec1      ; go get the start record's byte count
               cmp.b    #'9',D2        ; 16 bit start address record?
               beq      loadSRec1      ; go get the start record's byte count
               lea      msgInvalidRec,A0
               bsr      puts           ; print "Invalid Record Type"
               bra      loadSRec99     ; and exit

; get the record's byte count
loadSRec1:     bsr      get2hex        ; get the next two hex digits (byte count) from the buffer
               move.b   D0,D3          ; save the record's byte count in D3
               subq.b   #1,D3          ; reduce the byte count by 1 (for the checksum)

               ; get the record's address
               cmp.b    #'2',D2        ; is this a 24 bit data record?
               beq      loadSRec2      ; if so, go get the address
               cmp.b    #'8',D2        ; is this a 24 bit start record?
               beq      loadSRec2      ; if so, go get the address
               cmp.b    #'3',D2        ; is this a 32 bit data record?
               beq      loadSRec3      ; if so, go get the address
               cmp.b    #'7',D2        ; is this a 32 bit start record?
               beq      loadSRec3      ; if so, go get the address

; get the 16 bit record's address
               bsr      get4hex        ; get the next four hex digits (16 bit address) from the buffer into D0
               andi.l   #$0000FFFF,D0
               bra      loadSRec4

; get the 24 bit record's address
loadSRec2:     bsr      get6hex        ; get the next six hex digits (24 bit address) from the buffer into D0
               andi.l   #$00FFFFFF,D0
               bra      loadSRec4

; get the 32 bit record's address
loadSRec3:     bsr      get8hex        ; get the next eight hex digits (32 bit address) from the buffer into D0
loadSRec4:     move.l   D0,A3          ; save the record's address in A3

; get the record's data bytes
loadSRec5:     cmp.b    #'7',D2        ; is this a start record?
               bge      loadSRec7      ; skip data bytes and go get the checksum if start record
loadSRec6:     cmp.b    #0,D3          ; else, is the byte count in zero?
               beq      loadSRec7      ; if yes, finished with data bytes, go get the checksum
               bsr      get2hex        ; else, get the next two hex digits (data byte) from the buffer into D0
               move.b   D0,(A3)+       ; store the data byte from the record to the address in A3
               bra      loadSRec6      ; go back for the next data byte

; get the record's checksum
loadSRec7:     bsr      get1hex        ; get first hex digit of the checksum
               asl.b    #4,D0          ; move it to most significant nybble position
               move.b   D0,D1          ; save most significant nybble in D1
               bsr      get1hex        ; get second hex digit of the checksum
               add.b    D1,D0          ; combine most significant and least significant nybbles into D0
               not.b    D4             ; convert the checksum into one's complement
               cmp.b    D4,D0          ; compare the computed checksum in D4 to the record's checksum in D0
               beq      loadSRec99     ; branch if they're the same
               lea      msgCkSumError,A0
               bsr      puts           ; else, print 'Checksum error!'
loadSRec99:    movem.l  (SP)+,D0/A0    ; restore registers
               rts

; buffer input routines used by the 'loadSRec' function
; get the next hexadecimal digit from the buffer. return the nybble in D0
get1hex:       move.b   (A0)+,D0       ; get the next character from the buffer
               sub.b    #$30,D0        ; convert ASCII digit to binary
               cmp.b    #$09,D0        ; test for number (0-9)
               ble      get1hex1       ; if number then exit
               sub.b    #$07,D0        ;  else, convert letter A-F to binary
get1hex1:      rts                     ; and return

; get the next two hexadecimal digits from the buffer. return the byte in D0
; add the byte to the checksum in D4 and decrement the byte count in D3
get2hex:       bsr      get1hex        ; get first hex character of the byte
               asl.b    #4,D0          ; move it to the most significant nybble position
               move.b   D0,D1          ; save most significant nybble in D1
               bsr      get1hex        ; get second hex character of the byte
               add.b    D1,D0          ; merge most significant and least significant nybbles
               add.b    D0,D4          ; add the byte to the checksum in D4
               subq.b   #1,D3          ; decrement the byte count in D3
               rts

; get the next four hexadecimal digits from the buffer. return the 16 bit address in D0
get4hex:       bsr      get2hex        ; get the upper order byte
               asl.w    #8,D0          ; move it to most significant position
               bra      get2hex        ; get least significant byte and return

;get the next six hexadecimal digits from the buffer. return the 24 bit address in D0
get6hex:       bsr      get2hex        ; get upper order byte
               swap     D0             ; move it to most significant position
               bra      get4hex        ; get lower order word and return

;get the next eight hexadecimal digits from the buffer. return the 32 bit address in D0
get8hex:       bsr      get4hex        ; get upper order word
               swap     D0             ; move it to most significant position
               bra      get4hex        ; get lower order word and return

;============================================================================
; print the uptime as HH:MM:SS
;============================================================================
uptimeCMD:     cmp.b    #10,hrs
               bge      uptime1
               move.b   #'0',D0
               jsr      putc
uptime1:       clr.l    D0
               move.b   hrs,D0
               jsr      printDec       ; print hours
               move.b   #':',D0
               jsr      putc

               cmp.b    #10,mins
               bge      uptime2
               move.b   #'0',D0
               jsr      putc
uptime2:       clr.l    D0
               move.b   mins,D0
               jsr      printDec       ; print minutes
               move.b   #':',D0
               jsr      putc

               cmp.b    #10,secs
               bge      uptime3
               move.b   #'0',D0
               jsr      putc
uptime3:       clr.l    D0
               move.b   secs,D0
               jsr      printDec       ; print seconds
               jsr      newline
               jsr      newline
               movem.l  (SP)+,D0/A0
               rts

;============================================================================
; FILL command
; fill a block of SRAM with a byte
; f ADDR BYTE COUNT
;============================================================================
fillCMD:       bsr      parseNumber    ; read the address from the buffer
               tst.b    D1             ; is it a valid number?
               bne      invalidAddr
               move.l   D0,-(SP)       ; push the address

               bsr      parseNumber    ; read the value from the buffer
               tst.b    D1             ; is it a valid number?
               bne      invalidVal
               cmp.l    #255,D0        ; make sure it's a byte
               bgt      invalidVal
               move.l   D0,-(SP)       ; push the value

               bsr      parseNumber    ; read the count from the buffer
               tst.b    D1             ; is it a valid number?
               bne      invalidVal
               cmp.l    #65535,D0      ; make sure it's a word
               bgt      invalidVal
               move.l   D0,-(SP)       ; push the count

               move.l  (SP)+,D6        ; pop the count into D6
               move.l  (SP)+,D7        ; pop the value into D7
               move.l  (SP)+,A3        ; pop the address into A3

fill3:         move.b   D7,(A3)+
               subq.w   #1,D6
               bne      fill3
               movem.l  (SP)+,D0/A0    ; restore registers
               rts

;============================================================================
; EXAMINE command
; Valid modes:
;   e ADDR                  displays a single byte
;   e ADDR-ADDR             displays all bytes between the two addresses
;   e ADDR+LEN              dispays LEN bytes after ADDR
;   e ADDR;                 interactive mode, space shows 16 lines, ENTER shows 1.
;   e ADDR.                 quick line,displays one line
;============================================================================
examineCMD:    bsr      parseNumber    ; read in the start address
               tst.b    D1             ; make sure it's valid (parseNumber returns non-zero in D1 for failure)
               bne      invalidAddr
               move.l   D0,A3          ; save the start address
examine1:      move.b   (A0)+,D0
               cmp.b    #' ',D0        ; ignore spaces
               beq      examine1
               cmp.b    #'-',D0        ; check if it's a range specifier
               beq      examine2
               cmp.b    #'+',D0        ; check if it's a length specifier
               beq      examine4
               cmp.b    #';',D0        ; check if we're going interactive
               beq      examine6
               cmp.b    #'.',D0        ; check if quick 16
               beq      examine3
               move.l   #1,D0          ; otherwise read in a single byte
               bra      examine5

examine2:      bsr      parseNumber    ; find the end address
               tst.b    D1             ; check if we found a valid address
               bne      invalidAddr
               sub.l    A3,D0          ; get the length
               bra      examine5

; quick mode means show one line of 16 bytes
examine3:      move.l   #$10,D0
               bra      examine5

; length mode means a length is specified
examine4:      bsr      parseNumber    ; find the length
               tst.b    D1
               bne      invalidAddr
; done parsing, give the parameters to dumpRAM and exit
examine5:      move.l   A3,A0
               bsr      dumpRAM
               movem.l  (SP)+,D0/A0    ; restore registers
               rts

; interactive mode, space shows 16 lines, enter shows 1.
examine6:      move.l   A3,A0          ; current Address
               move.l   #$10,D0        ; 16 bytes
               bsr      dumpRAM        ; dump this line
               add.l    #$10,A3        ; move up the current address 16 bytes
examine7:      bsr      getc
               cmp.b    #CR,D0         ; display another line
               beq      examine6
               cmp.b    #' ',D0        ; display a page (256 bytes at a time)
               beq      examine8
               movem.l  (SP)+,D0/A0    ; restore registers
               rts

examine8:      move.l   A3,A0
               move.l   #$100,D0       ; 256 bytes
               bsr      dumpRAM        ; dump 16 lines of RAM
               add.l    #$100,A3       ; move up the current address by 256
               bra      examine7

;============================================================================
; DEPOSIT command
; d ADDR VAL VAL     deposit value(s) into RAM
; d ADDR VAL VAL;    deposit values, continue with values on next line
;  VAL VAL VAL;      - continuing with further continue
; d: VAL VAL         continue depositing values after the last address written to
;============================================================================
depositCMD:    move.b   (A0),D0
               cmp.b    #':',D0        ; check if we want to continue from last
               beq      deposit2
               bsr      parseNumber    ; otherwise read the address
               tst.b    D1
               bne      invalidAddr
               move.l   D0,A3          ; save the start address
deposit1:      move.b   (A0),D0
               cmp.b    #';',D0        ; check for continue
               beq      deposit3
               tst      D0             ; check for the end of line
               beq      deposit4
               bsr      parseNumber    ; otherwise read a value
               tst.b    D1
               bne      invalidVal
               cmp.w    #255,D0        ; make sure it's a byte
               bgt      invalidVal
               move.b   D0,(A3)+       ; store the value into memory
               bra      deposit1

deposit2:      move.l   lineBuffPtr,A3 ; read in the last address
               addq.l   #1,A0          ; skip over the ':'
               bra      deposit1

deposit3:      lea      msgColonSpace,A0
               bsr      puts
               bsr      readLine       ; read in the next line to be parsed
               bsr      lineToUpper    ; convert to uppercase
               lea      lineBuffer,A0  ; reset our buffer pointer
               bra      deposit1       ; and jump back to decoding

deposit4:      move.l   A3,lineBuffPtr
               movem.l  (SP)+,D0/A0    ; restore registers
               rts

;============================================================================
; RUN command
;============================================================================
runCMD:        bsr      parseNumber    ; otherwise read the address
               tst.b    D1
               bne      invalidAddr
               move.l   D0,A0
               jsr      (A0)           ; go as subroutine to allow code to return to us
               movem.l  (SP)+,D0/A0    ; restore registers
               rts

;============================================================================
; HELP command
;============================================================================
helpCMD:       lea      msgHelp,A0
               bsr      puts
               movem.l  (SP)+,D0/A0    ; restore registers
               rts

;============================================================================
; BASIC command
;============================================================================
runBASIC:      lea      $084800,A0
               ori.w	   #$0700,SR		; disable interrupts
               move.b   #$00,LEDs      ; turn off yellow LEDs
               move.b	#$80,OPR		   ; turn off orange LED               
               jsr      (A0)
               movem.l  (SP)+,D0/A0    ; restore registers
               rts

;----------------------------------------------------------------------------
; find and parse a hex number from the input buffer
; starting address in A0
; number returned in D0
; status in D1 (0 success, 1 fail)
;----------------------------------------------------------------------------
parseNumber:   eor.l    D0,D0          ; zero out D0
               move.b   (A0)+,D0
               cmp.b    #' ',D0        ; ignore all leading spaces
               beq      parseNumber
               cmp.b    #'0',D0        ; look for hex digits 0-9
               blt      parseNumber1
               cmp.b    #'9',D0
               ble      parseNumber3
               cmp.b    #'A',D0        ; look for hex digits A-F
               blt      invalidCMD
               cmp.b    #'F',D0
               ble      parseNumber2
parseNumber1:  move.l   #1,D1          ; invalid character, mark failure and return
               rts

parseNumber2:  sub.b    #'7',D0        ; turn 'A' to 10
               bra      parseNumber4
parseNumber3:  sub.b    #'0',D0        ; turn '0' to 0
parseNumber4:  move.b   (A0)+,D1       ; read in a digit
               cmp.b    #'0',D1        ; look for hex digits 0-9
               blt      parseNumber5   ; any other characters mean we're done reading
               cmp.b    #'9',D1
               ble      parseNumber7
               cmp.b    #'A',D1        ; look for hex digits A-F
               blt      parseNumber5
               cmp.b    #'F',D1
               ble      parseNumber6

; we hit a non-hex digit character, we're done parsing
parseNumber5:  subq.l   #1,A0          ; move the pointer back before the end character we read
               clr.l    D1
               rts

parseNumber6:  sub.b    #'7',D1        ; turn 'A' to 10
               bra      parseNumber8

parseNumber7:  sub.b    #'0',D1        ; turn '0' to 0
parseNumber8:  lsl.l    #4,D0          ; shift over to the next nybble
               add.b    D1,D0          ; place in our current nybble (could be or.b instead)
               bra      parseNumber4

;----------------------------------------------------------------------------
; dumps a section of RAM to the console
; displays both hex values and ASCII characters
; D0 - number of bytes to dump
; A0 - start Address
;----------------------------------------------------------------------------
dumpRAM:       movem.l  D2-D4/A2,-(SP) ; save registers
               move.l   A0,A2          ; save the start address
               move.l   D0,D2          ; and the number of bytes
dumpRAM1:      move.l   A2,D0
               bsr      printHexAddr   ; starting address of this line
               lea      msgColonSpace,A0
               bsr      puts
               move.l   #16,D3         ; 16 bytes can be printed on a line
               move.l   D3,D4          ; save number of bytes on this line
dumpRAM2:      tst.l    D2             ; check if we're out of bytes
               beq      dumpRAM3
               tst.b    D3             ; check if we're done this line
               beq      dumpRAM5
               move.b   (A2)+,D0       ; read a byte in from RAM
               bsr      printHexByte   ; display it
               move.b   #' ',D0
               bsr      putc           ; space out bytes
               subq.l   #1,D3
               subq.l   #1,D2
               bra      dumpRAM2
dumpRAM3:      sub.b    D3,D4          ; make D4 the actual number of bytes on this line
               move.b   #' ',D0
dumpRAM4:      tst.b    D3             ; check if we ended the line
               beq      dumpRAM5
               move.b   #' ',D0
               bsr      putc           ; three spaces to pad out
               move.b   #' ',D0
               bsr      putc
               move.b   #' ',D0
               bsr      putc
               subq.b   #1,D3
               bra     dumpRAM4

dumpRAM5:      suba.l   D4,A2          ; return to the start address of this line
dumpRAM6:      tst.b    D4             ; check if we are done printing ascii
               beq      dumpRAM8
               subq.b   #1,D4
               move.b   (A2)+,D0       ; read the byte again
               cmp.b    #' ',D0        ; lowest printable character
               blt      dumpRAM7
               cmp.b    #'~',D0        ; highest printable character
               bgt      dumpRAM7
               bsr      putc
               bra      dumpRAM6

dumpRAM7:      move.b   #'.',D0
               bsr      putc
               bra      dumpRAM6

dumpRAM8:      jsr      newline
               tst.l    D2
               ble      dumpRAM9
               bra      dumpRAM1

dumpRAM9:      movem.l  (SP)+,D2-D4/A2 ; restore registers
               rts

;----------------------------------------------------------------------------
; print the contents of D0 as a hex word
;----------------------------------------------------------------------------
printHexWord:  move.l   D2,-(SP)       ; save D2
               move.l   D0,D2          ; save the address in D2
               rol.l    #$8,D2         ; 4321 -> 3214
               rol.l    #$8,D2         ; 3214 -> 2143
               bra      printHexLong2  ; print out the last 16 bits

;----------------------------------------------------------------------------
; print the contents of D0 as a hex 24-bit address
;----------------------------------------------------------------------------
printHexAddr:  move.l   D2,-(SP)       ; save D2
               move.l   D0,D2          ; save the address in D2
               rol.l    #$8,D2         ; 4321 -> 3214
               bra      printHexLong1  ; print out the last 24 bits

;----------------------------------------------------------------------------
; print the contents of D0 as a hex long
;----------------------------------------------------------------------------
printHexLong:  move.l   D2,-(SP)       ; save D2
               move.l   D0,D2          ; save the address in D2
               rol.l    #$8,D2         ; 4321 -> 3214 high byte in low
               move.l   D2,D0
               bsr      printHexByte   ; print the high byte (24-31)
printHexLong1: rol.l    #$8,D2         ; 3214 -> 2143 middle-high byte in low
               move.l   D2,D0
               bsr      printHexByte   ; print the high-middle byte (16-23)
printHexLong2: rol.l    #$8,D2         ; 2143 -> 1432 Middle byte in low
               move.l   D2,D0
               bsr      printHexByte   ; print the middle byte (8-15)
               rol.l    #$8,D2
               move.l   D2,D0
               bsr      printHexByte   ; print the low byte (0-7)
               move.l   (SP)+,D2       ; restore D2
               rts

;----------------------------------------------------------------------------
; print the contents of D0 as a hex byte
;----------------------------------------------------------------------------
printHexByte:  movem.l  D0-D2,-(SP)
               clr.l    D2
               move.b   D0,D2
               lsr.b    #$04,D0        ; shift down upper nibble
               andi.b   #$0F,D0
               add.b    #'0',D0
               cmp.b    #'9',D0        ; check if the hex number was from 0-9
               ble.s    printHexByte1
               add.b    #$07,D0        ; shift $A-$F from ':' to 'A'
printHexByte1: bsr      putc           ; print the digit
               andi.b   #$0F,D2        ; now we want the lower digit Mask only the lower digit
               add.b    #'0',D2
               cmp.b    #'9',D2        ; same as before
               ble.s    printHexByte2
               add.b    #$07,D2
printHexByte2: move.b   D2,D0
               bsr      putc           ; print the lower digit
               movem.l  (SP)+,D0-D2
               rts

;----------------------------------------------------------------------------
; print the contents of D0 as a decimal number.
;----------------------------------------------------------------------------
printDec:      movem.l D0-D3,-(SP)     ; save registers
               move.w #$FFFF,-(SP)     ; push -1 onto the stack
               tst.l D0                ; zero?
               beq printDec3           ; yes, make sure we output a zero
; divide by ten to pick up digit value as remainder
; keep doing this until number is zero
printDec1:     tst.l D0                ; finished?
               beq printDec2           ; yes, output the number
               move.w D0,D2            ; save low orger word in D2
               clr.w D0                ; clear low order word
               swap D0                 ; get high order word in low order word
               divu #10,D0             ; divide by 10
               move.w D0,D3            ; save remainder in D3
               move.w D2,D0            ; get low order word back
               divu #10,D0             ; divide by 10
               swap D0                 ; swap quotient and remainder
               move.w D0,-(SP)         ; save remainder as digit value
               move.w D3,D0            ; get old remainder in low order word
               swap D0                 ; fix up so result is full 32 bit quotient
               bra printDec1           ; divide by 10 again

; we now output the number
printDec2:     move.w (SP)+,D0         ; get a digit from the stack
               bmi printDec4           ; terminate on -1
               and.l #$0F,D0            ; mask
printDec3:     add.b #'0',D0           ; convert digit to ASCII character
               jsr putc                ; output
               bra printDec2           ; continue to next digit

printDec4:     movem.l (SP)+,D0-D3     ; restore registers
               rts

;----------------------------------------------------------------------------
; writes a character in D0 to Port A
;----------------------------------------------------------------------------
putc:          btst     #2,SRA         ; check the transmitter ready bit
               beq      putc
               move.b   D0,TBA         ; transmit character
               rts

;----------------------------------------------------------------------------
; print a null terminated string
;----------------------------------------------------------------------------
puts:          move.b   (A0)+,D0       ; get the next character from the string
               beq      puts1          ; check for the null
               bsr      putc           ; else, write the character
               bra      puts
puts1:         rts

;----------------------------------------------------------------------------
; print carriage return and linefeed to the console
;----------------------------------------------------------------------------
newline:       move.l   D0,-(SP)       ; save D0
               move.b   #CR,D0
               jsr      putc
               move.b   #LF,D0
               jsr      putc
               move.l   (SP)+,D0       ; restore D0
               rts
               
;----------------------------------------------------------------------------
; return the next character from the Rx Buffer in D0
; if the 'blink' flag is set, blink the orange LED at about 2Hz while waiting for input.
; if that's too annoying, ^B toggles the 'blink' flag.
;----------------------------------------------------------------------------
getc:          movem.l  A0,-(SP)       ; save A0
getc1:         move.l   inPtr,D0
               cmp.l    outPtr,D0
               bne      getc3

               btst.b   #7,flags
               beq      getc1
               addq.w   #1,LEDcounter
               cmp.w    #flashCount,LEDcounter
               beq      getc2
               cmp.w    #flashCount*2,LEDcounter
               bne      getc1
               move.b	#$80,OPS		   ; turn on orange LED
               clr.w    LEDcounter
               bra      getc1

getc2:         move.b	#$80,OPR		   ; turn off orange LED
               bra      getc1

getc3:         lea      RxBuffer,A0    ; address of Rx buffer
               adda.l   (outPtr),A0    ; add the pointer to the address
               move.b   (A0),D0        ; retrieve the character from the buffer
               addq.l   #1,outPtr      ; update the pointer
               andi.l   #$FF,outPtr    ; limit the buffer to 256 bytes
               cmp.b    #$02,D0        ; was the last character ^B?
               bne      getc4          ; nope, exit
               
               eori.b   #$80,flags     ; toggle the 'blink' flag
               btst.b   #7,flags       ; is the 'blink' flag set?
               bne      getc1          ; if yes, go back for another character
               move.b	#$80,OPR		   ; else, turn off orange LED
               bra      getc1
               
getc4:         movem.l  (SP)+,A0       ; restore A0
               rts

;----------------------------------------------------------------------------
; DUART Channel A Rx Interrupt Service Routine
;----------------------------------------------------------------------------
RxISR:         movem.l  D0/A0,-(SP)    ; save registers
               btst     #1,ISR         ; test channel A receiver ready bit
               beq      TimerISR
               move.b	RBA,D0		   ; get the character from the DUART
               lea      RxBuffer,A0    ; load the address of the Rx buffer
               adda.l   (inPtr),A0     ; add the pointer to the address
               move.b   D0,(A0)        ; store the character in the Rx buffer
               addq.l   #1,inPtr       ; update the pointer
               andi.l   #$FF,inPtr     ; limit the buffer to 256 bytes

;----------------------------------------------------------------------------
; DUART Timer Interrupt Service Routine
;----------------------------------------------------------------------------
TimerISR:      btst     #3,ISR         ; test Counter/Timer bit
               beq      TimerExit
               move.b   OPR,D0		   ; read of OPR clears interrupt
               addq.b   #1,ticks		   ; increment the ticks counter
               cmp.b    #60,ticks
               bne      TimerExit
               clr.b    ticks
              ;move.b   secs,LEDs
               addq.b   #1,secs        ; increment the seconds counter
               cmp.b    #60,secs
               bne      TimerExit
               clr.b    secs
               addq.b   #1,mins        ; increment the minutes counter
               cmp.b    #60,mins
               bne      TimerExit
               clr.b    mins
               addq.b   #1,hrs         ; increment the hours counter
TimerExit:     movem.l  (SP)+,D0/A0	   ; restore registers
               rte

;----------------------------------------------------------------------------
; bus errors, address errors, illegal instructions, division by zero, etc, end up here.
;----------------------------------------------------------------------------
error:         reset

;----------------------------------------------------------------------------
msgBanner:     dc.b     "\e[2J\e[H"
               dc.b     "68008 SBC Serial Monitor\r\n"
               dc.b     "Assembled on ",DATE," at ",TIME,"\r\n\n",0
msgInvalidCMD: dc.b     "Invalid Command\r\n",0
msgHelp:       dc.b     "Available Commands:\r\n"
               dc.b     "  ",SGR4,"B",SGR0,"ASIC"
               dc.b     "  ",SGR4,"D",SGR0,"eposit"
               dc.b     "  ",SGR4,"E",SGR0,"xamine"
               dc.b     "  ",SGR4,"F",SGR0,"ill"
               dc.b     "  ",SGR4,"H",SGR0,"elp"
               dc.b     "  ",SGR4,"R",SGR0,"un"
               dc.b     "  ",SGR4,"S",SGR0,"REC"
               dc.b     "  ",SGR4,"U",SGR0,"ptime\r\n\n",0
msgInvalidAddr:dc.b     "Invalid Address\r\n",0
msgInvalidVal: dc.b     "Invalid Value\r\n",0
msgInvalidRec: dc.b     "Invalid Record Type\r\n",0
msgCkSumError: dc.b     "Checksum error!\r\n",0
msgPrompt:     dc.b     "> ",0
msgColonSpace: dc.b     ": ",0

; Lee Davidson's Enhanced BASIC
               INCLUDE  "basic68k.inc"
