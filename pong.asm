org 0x100    
section .data
  
    border_color equ 0x03 
    paddle_color equ 0x02  
    ball_color   equ 0x0F 
    score_color equ 0x0E 


    paddle_x dw 130      
    paddle_y dw 170     
    paddle_width equ 40  


    ball_x dw 160
    ball_y dw 100
    ball_dx dw 1      
    ball_dy dw 1        

    ball_colors db 0x0F, 0x0C, 0x0A, 0x0E, 0x09 
    ball_color_index db 0

   
    screen_left equ 50
    screen_right equ 270
    screen_top equ 30
    screen_bottom equ 170
  
    score dw 0

    win_message db 'You Won', 0

   
    MAX_BLOCKS equ 10          
    BLOCK_WIDTH equ 15         
    BLOCK_HEIGHT equ 15        
    BLOCK_STRUCT_SIZE equ 6    
    blocks times MAX_BLOCKS*BLOCK_STRUCT_SIZE db 0

    hits_since_last_block db 0  
    hits_needed_for_block db 3 
    last_tick dw 0             


section .text
main:
    mov ax, 0x13        ; Set VGA 320x200 mode
    int 0x10

    call init_blocks    ; Initialize block system
    call draw_borders
    call draw_paddle
    call draw_ball
    call draw_score

game_loop:
    ; Check keyboard
    mov ah, 0x01
    int 0x16
    jz no_key_press
    
    mov ah, 0x00
    int 0x16
    
    cmp ah, 0x4B        ; Left arrow
    je move_left
    cmp ah, 0x4D        ; Right arrow
    je move_right
    cmp al, 0x1B        ; ESC key
    je exit_game

no_key_press:
    call update_ball
    call update_blocks
    ; call check_block_collision
    call draw_blocks
    call draw_score
    call delay
    jmp game_loop

move_left:
    cmp word [paddle_x], screen_left+5
    jl no_key_press

    ; Erase old paddle
    mov cx, [paddle_x]
    mov dx, [paddle_y]
    mov si, paddle_width
    mov al, 0x00
    call draw_horizontal_line

    sub word [paddle_x], 5
    call draw_paddle
    jmp no_key_press

move_right:
    mov ax, screen_right
    sub ax, paddle_width
    sub ax, 5
    cmp word [paddle_x], ax
    jg no_key_press

    ; Erase old paddle
    mov cx, [paddle_x]
    mov dx, [paddle_y]
    mov si, paddle_width
    mov al, 0x00
    call draw_horizontal_line

    add word [paddle_x], 5
    call draw_paddle
    jmp no_key_press

exit_game:
    mov ax, 0x0003      ; Return to text mode
    int 0x10
    mov ax, 0x4C00      ; Exit to DOS
    int 0x21


draw_borders:
    ; Top border
    mov cx, screen_left
    mov dx, screen_top
    mov si, screen_right - screen_left
    mov al, border_color
    call draw_horizontal_line

    ; Left border
    mov cx, screen_left
    mov dx, screen_top
    mov si, screen_bottom - screen_top
    call draw_vertical_line

    ; Right border
    mov cx, screen_right
    mov dx, screen_top
    mov si, screen_bottom - screen_top
    call draw_vertical_line
    ret

draw_paddle:
    mov cx, [paddle_x]
    mov dx, [paddle_y]
    mov si, paddle_width
    mov al, paddle_color
    call draw_horizontal_line
    ret

draw_ball:
    pusha
    mov cx, [ball_x]
    mov dx, [ball_y]
    mov si, 3       
    mov al, [ball_color] 

.draw_row:
    push cx
    mov di, 3       

.draw_col:
    mov ah, 0Ch
    int 10h
    inc cx
    dec di
    jnz .draw_col
    pop cx
    inc dx
    dec si
    jnz .draw_row
    popa
    ret

update_ball:
    ; Erase old ball
    pusha
    mov cx, [ball_x]
    ; dec cx
    mov dx, [ball_y]
    ; dec dx
    mov si, 3

.erase_row:
    push cx
    mov di, 3

.erase_col:
    mov ah, 0Ch
    mov al, 0x00
    int 10h
    inc cx
    dec di
    jnz .erase_col
    pop cx
    inc dx
    dec si
    jnz .erase_row
    popa
    
    mov ax, [ball_dx]
    add [ball_x], ax
    mov ax, [ball_dy]
    add [ball_y], ax

    call check_wall_collision
    call check_paddle_collision
    call check_block_collision

    call draw_ball
    ret

check_wall_collision:

    cmp word [ball_x], screen_left+1
    jg .next1
    neg word [ball_dx]
.next1:
    cmp word [ball_x], screen_right-3
    jl .next2
    neg word [ball_dx]
    

.next2:
    cmp word [ball_y], screen_top+1
    jg .next3
    neg word [ball_dy]
    

.next3:
    cmp word [ball_y], screen_bottom-1
    jge exit_game

    ret
    
    

check_paddle_collision:
    pusha
    mov ax, [paddle_y]
    sub ax, 3
    cmp word [ball_y], ax
    jl no_collision
    
    mov ax, [paddle_x]
    cmp word [ball_x], ax
    jl no_collision
    
    add ax, paddle_width
    cmp word [ball_x], ax
    jg no_collision
    
    call get_random        ; Get random number in AL
    and al, 0x07          ; Limit to 0-7
    cmp al, 5             ; We have 5 colors (0-4)
    jb .color_ok
    mov al, 0             ; If >=5, use first color
.color_ok:
    mov [ball_color_index], al
    mov si, ball_colors
    add si, ax
    mov al, [si]
    mov [ball_color], al

    neg word [ball_dy]
    inc word [score]
    cmp word [score], 30
    je game_won
    
    ; Track paddle hits for block spawning
    inc byte [hits_since_last_block]
    jmp no_collision
    
no_collision:
    popa
    ret

draw_pixel:
    push ax
    push bx
    push cx
    push dx
    mov ah, 0x0C
    int 0x10
    pop dx
    pop cx
    pop bx
    pop ax
    ret

draw_horizontal_line:
    ; CX = X, DX = Y, SI = length, AL = color
    push cx
    push si

.h_line_loop:
    call draw_pixel
    inc cx
    dec si
    jnz .h_line_loop
    pop si
    pop cx
    ret

draw_vertical_line:
    ; CX = X, DX = Y, SI = length, AL = color
    push dx
    push si

.v_line_loop:
    call draw_pixel
    inc dx
    dec si
    jnz .v_line_loop
    pop si
    pop dx
    ret

delay:
    push cx
    mov cx, 0xFFFF
.delay_loop:
    loop .delay_loop
    pop cx
    ret

draw_score:
    pusha
    mov cx, 160
    mov dx, 16
    mov si, 40
    mov al, 0x00
    call draw_horizontal_line
    
    ; Set cursor position
    mov ah, 02h
    xor bh, bh
    mov dh, 2
    mov dl, 20
    int 10h

    ; Convert score to ASCII
    mov ax, [score]
    mov bx, 10
    xor cx, cx
    
    test ax, ax
    jnz .convert_loop
    mov dl, '0'
    call .draw_digit
    jmp .done

.convert_loop:
    xor dx, dx
    div bx
    add dl, '0'
    push dx
    inc cx
    test ax, ax
    jnz .convert_loop

.display_loop:
    pop dx
    call .draw_digit
    loop .display_loop

.done:
    popa
    ret

.draw_digit:
    pusha
    mov ah, 09h
    mov al, dl
    mov bh, 0
    mov bl, score_color
    mov cx, 1
    int 10h
    
    mov ah, 03h
    int 10h
    inc dl
    mov ah, 02h
    int 10h
    popa
    ret

game_won:
    pusha
    ; Clear score area
    mov cx, 160
    mov dx, 16
    mov si, 40
    mov al, 0x00
    call draw_horizontal_line

    ; Display "You Won"
    mov ah, 02h
    xor bh, bh
    mov dh, 2
    mov dl, 17
    int 10h

    mov si, win_message

.display_loop:
    lodsb
    test al, al
    jz .done
    mov ah, 09h
    mov bl, score_color
    mov cx, 1
    int 10h
    
    mov ah, 03h
    int 10h
    inc dl
    mov ah, 02h
    int 10h
    jmp .display_loop

.done:
    popa
.wait_esc:
    mov ah, 0x01
    int 0x16
    jz .wait_esc
    mov ah, 0x00
    int 0x16
    cmp al, 0x1B
    je exit_game
    jmp .wait_esc

init_blocks:
    pusha
    ; Clear all blocks
    mov di, blocks
    mov cx, MAX_BLOCKS*BLOCK_STRUCT_SIZE
    xor al, al
    rep stosb

    ; Initialize random seed
    mov ah, 0
    int 1Ah
    mov [last_tick], dx
    popa
    ret


    ; Returns: AL = random byte (0-255)
get_random:
    push cx
    push dx
    
    mov ah, 0x00       ; BIOS get system timer
    int 0x1A           ; Returns ticks in CX:DX
    
    add [last_tick], dx ; Update seed
    mov al, [last_tick] ; Use low byte for randomness
    rol al, 1          ; Rotate left for better randomization
    
    pop dx
    pop cx
    ret

    ; Input:  AL = min, BL = max
    ; Output: AL = random number [min,max]
get_random_range:
    push bx
    push cx
    push dx
    push ax
    

    sub bl, al     
    mov bh, bl   
    inc bh        
    
    call get_random
    
   
    xor ah, ah    
    div bh         

    xor dx, dx
    mov dl, ah    
    pop ax
    add ax, dx
    
    pop dx
    pop cx
    pop bx
    ret

spawn_block:
    pusha
    mov cx, MAX_BLOCKS
    mov si, blocks

.find_slot:
    cmp byte [si+5], 0
    je .found_slot
    add si, BLOCK_STRUCT_SIZE
    loop .find_slot
    jmp .done

.found_slot:
    ; Random X position
    mov ax, screen_left
    add ax, 5
    mov bx, screen_right - BLOCK_WIDTH
    sub bx, 5
    call get_random_range
    mov [si], ax

    ; Random Y position
    mov ax, screen_top
    add ax, 5
    mov bx, screen_bottom - BLOCK_HEIGHT
    sub bx, 10
    call get_random_range
    mov [si+2], ax

    ; Random stamina
    mov ax, 1
    mov bx, 5
    call get_random_range
    mov [si+4], al

    mov byte [si+5], 1

.done:
    popa
    ret

draw_blocks:
    pusha
    mov cx, MAX_BLOCKS    
    mov si, blocks        

.draw_loop:
    cmp byte [si+5], 0      
    je .next_block

   
    mov al, [si+4]        
    add al, 0x0A             ; Convert to color (0x0B-0x0F)

   
    push cx

   
    mov cx, [si]           
    mov dx, [si+2]         
    mov di, BLOCK_HEIGHT   

.draw_rows:
    push cx                
    push di                
    mov di, BLOCK_WIDTH    

.draw_cols:
  
    ; CX = current X, DX = current Y, AL = color
    ; BH = 0 (page number) assumed from earlier
    call draw_pixel             
    
    inc cx                
    dec di
    jnz .draw_cols         

    pop di                
    pop cx                
    inc dx                
    dec di
    jnz .draw_rows        

    mov al, [si+4]       
    sub al, 1
    mov cx, [si]        
    add cx, (BLOCK_WIDTH-5)/2 
    mov dx, [si+2]       
    add dx, (BLOCK_HEIGHT-5)/2 
    call draw_digit_pixel

    pop cx                 

.next_block:
    add si, BLOCK_STRUCT_SIZE 
    loop .draw_loop

    popa
    ret

check_block_collision:
    pusha
    mov cx, MAX_BLOCKS
    mov si, blocks

.block_loop:
    cmp byte [si+5], 0  
    je .next_block
    ; ---- Check 0: collision on the corner ----
    mov ax, [ball_x]
    mov bx, [si]
    add bx, BLOCK_WIDTH
    cmp ax, bx
    jne .next_corner
    mov ax, [ball_y]
    mov bx, [si+2]
    add bx, BLOCK_HEIGHT
    cmp ax, bx
    je .next_block
    add ax, 3
    sub bx, BLOCK_HEIGHT
    cmp ax, bx
    je .next_block
.next_corner:
    mov ax, [ball_x]
    add ax, 3
    mov bx, [si]
    cmp ax, bx
    jne .corner_collision
    mov ax, [ball_y]
    mov bx, [si+2]
    add bx, BLOCK_HEIGHT
    cmp ax, bx
    je .next_block
    add ax, 3
    sub bx, BLOCK_HEIGHT
    cmp ax, bx
    je .next_block

.corner_collision:
    mov ax, [ball_x]
    mov bx, [si]
    add bx, BLOCK_WIDTH
    sub bx, 1
    cmp ax, bx
    jne .next_corner_collision
    mov ax, [ball_y]
    mov bx, [si+2]
    add bx, BLOCK_HEIGHT
    sub bx, 1
    cmp ax, bx
    je .corner_collision_detected
    add ax, 2
    sub bx, BLOCK_HEIGHT
    add bx, 1
    cmp ax, bx
    je .corner_collision_detected
.next_corner_collision:
    mov ax, [ball_x]
    add ax, 2
    mov bx, [si]
    cmp ax, bx
    jne .next_left
    mov ax, [ball_y]
    mov bx, [si+2]
    add bx, BLOCK_HEIGHT
    sub bx, 1
    cmp ax, bx
    je .corner_collision_detected
    add ax, 2
    sub bx, BLOCK_HEIGHT
    add bx, 1
    cmp ax, bx
    je .corner_collision_detected

    jmp .next_left
.corner_collision_detected:
    neg word [ball_dx]
    neg word [ball_dy]
    dec byte [si+4]      
    jz .destroy_block    
    popa
    ret
    ; ---- Check 1: Ball left vs Block right ----
.next_left:
    mov ax, [ball_x]
    add ax, 3         
    cmp ax, [si]      
    jne .next_right     

    mov ax, [ball_y]
    ; sub ax, 1
    mov bx, [si+2]
    add bx, BLOCK_HEIGHT
    sub bx, 1
    cmp ax, bx
    jg .next_right
    add ax, 2
    sub bx, BLOCK_HEIGHT
    add bx, 1
    cmp ax, bx
    jl .next_right

    neg word [ball_dx]
    dec byte [si+4]     
    jz .destroy_block   
    popa
    ret
    ; ---- Check 2: Ball right vs Block left ----
.next_right:    
    mov ax, [ball_x]
    ; sub ax, 1            
    mov bx, [si]
    add bx, BLOCK_WIDTH   
    ; sub bx, 1
    cmp ax, bx           
    jne .next_top      

    mov ax, [ball_y]
    ; sub ax, 1
    mov bx, [si+2]
    add bx, BLOCK_HEIGHT
    sub bx, 1
    cmp ax, bx
    jg .next_top
    add ax, 2
    sub bx, BLOCK_HEIGHT
    add bx, 1
    cmp ax, bx
    jl .next_top

    neg word [ball_dx]
    dec byte [si+4]      
    jz .destroy_block    
    popa
    ret
    ; ---- Check 3: Ball top vs Block bottom ----
.next_top:
    mov ax, [ball_y]
    add ax, 3          
    cmp ax, [si+2]      
    jne .next_bottom     

    mov ax, [ball_x]
    ; sub ax, 1
    mov bx, [si]
    add bx, BLOCK_WIDTH
    sub bx, 1
    cmp ax, bx
    jg .next_bottom
    add ax, 2
    sub bx, BLOCK_WIDTH
    add bx, 1
    cmp ax, bx
    jl .next_bottom

    neg word [ball_dy]
    dec byte [si+4]      
    jz .destroy_block     
    popa
    ret

    ; ---- Check 4: Ball bottom vs Block top ----
.next_bottom:
    mov ax, [ball_y]
    ; sub ax, 1           
    mov bx, [si+2]
    add bx, BLOCK_HEIGHT  
    ; sub bx, 1
    cmp ax, bx           
    jne .next_block      

    mov ax, [ball_x]
    ; sub ax, 1
    mov bx, [si]
    add bx, BLOCK_WIDTH
    sub bx, 1
    cmp ax, bx
    jg .next_block
    add ax, 2
    sub bx, BLOCK_WIDTH
    add bx, 1
    cmp ax, bx
    jl .next_block

    neg word [ball_dy]
    dec byte [si+4]    
    jz .destroy_block   
    popa
    ret

.destroy_block:
    mov byte [si+5], 0   
    push cx
    mov al, 0x00 

    mov cx, [si]          
    mov dx, [si+2]        
    mov di, BLOCK_HEIGHT  

.draw_rows:
    push cx               
    push di               
    mov di, BLOCK_WIDTH   

.draw_cols:
    mov ah, 0x0C
    int 0x10                ; Draw pixel
    inc cx                
    dec di
    jnz .draw_cols        
    pop di                
    pop cx                
    inc dx                
    dec di
    jnz .draw_rows        

    pop cx
    popa
    ret
.next_block:
    add si, BLOCK_STRUCT_SIZE
    dec cx
    jnz near .block_loop
    popa
    ret

update_blocks:
    mov al, [hits_since_last_block]
    cmp al, [hits_needed_for_block]
    jb .no_spawn

    call spawn_block
    mov byte [hits_since_last_block], 0

  
    mov ax, 1
    mov bx, 3
    call get_random_range
    mov [hits_needed_for_block], al

.no_spawn:
    ret

draw_digit_pixel:
    pusha
   
    ; Look up digit pattern
    mov si, digit_patterns
    mov ah, 0
    imul ax, 5           
    add si, ax
    
    ; Draw digit (5x5 pixels)
    mov di, 5              ; Height counter
    mov bl, 0x00          ; black color for digits
    
.draw_row:
    push cx
    mov ah, [si]           ; Get pattern byte
    mov bh, 5              ; Width counter
    
.draw_col:
    test ah, 0x80          ; Check leftmost bit
    jz .skip
    mov al, bl          
    call draw_pixel       
.skip:
    shl ah, 1            
    inc cx                
    dec bh
    jnz .draw_col
    
    pop cx
    inc si             
    inc dx             
    dec di
    jnz .draw_row
    
.done:
    popa
    ret

digit_patterns:
    db 0b00100000, 0b01100000, 0b00100000, 0b00100000, 0b01110000   ; 1
    db 0b11111000, 0b00001000, 0b11111000, 0b10000000, 0b11111000   ; 2
    db 0b11111000, 0b00001000, 0b01111000, 0b00001000, 0b11111000   ; 3
    db 0b10001000, 0b10001000, 0b11111000, 0b00001000, 0b00001000   ; 4
    db 0b11111000, 0b10000000, 0b11111000, 0b00001000, 0b11111000   ; 5

