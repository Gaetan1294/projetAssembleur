; ===============================================
; Projet Triangle - Architecture 64 bits
; NASM x86-64 pour Linux avec X11
; ===============================================

section .data
    ; Constantes pour les limites de la fenêtre
    MAX_X equ 400
    MAX_Y equ 400
    
    ; Coordonnées du triangle courant
    ax_coord: dq 0
    ay_coord: dq 0
    bx_coord: dq 0
    by_coord: dq 0
    cx_coord: dq 0
    cy_coord: dq 0
    
    ; Pour stocker plusieurs triangles
    MAX_TRIANGLES equ 10
    triangles: times (MAX_TRIANGLES * 6) dq 0  ; 6 coordonnées par triangle (64 bits chacune)
    couleurs: times MAX_TRIANGLES dd 0          ; Couleurs RGB
    nb_triangles_a_dessiner: dq 5
    
    ; Variable pour générateur aléatoire
    seed: dq 0x123456789ABCDEF

    ; Variables X11 (à copier depuis votre code_pour_dessiner.asm)
    display_name: dq 0
    window: dq 0
    gc: dq 0
    screen: dq 0

section .text
    global _start
    
    ; Déclarer les fonctions X11 externes
    extern XOpenDisplay
    extern XDefaultScreen
    extern XRootWindow
    extern XBlackPixel
    extern XWhitePixel
    extern XCreateSimpleWindow
    extern XSelectInput
    extern XMapWindow
    extern XCreateGC
    extern XSetForeground
    extern XDrawPoint
    extern XDrawLine
    extern XFlush
    extern XCloseDisplay
    extern exit

_start:
    ; ===== Initialisation X11 (adapté de votre code) =====
    xor rdi, rdi
    call XOpenDisplay
    test rax, rax
    jz .exit_error
    mov qword[display_name], rax

    mov rdi, qword[display_name]
    call XDefaultScreen
    mov dword[screen], eax

    mov rdi, qword[display_name]
    mov esi, dword[screen]
    call XRootWindow
    mov qword[window], rax  ; Temporaire pour root window

    mov rdi, qword[display_name]
    mov esi, dword[screen]
    call XBlackPixel
    push rax                ; Sauvegarder black pixel

    mov rdi, qword[display_name]
    mov esi, dword[screen]
    call XWhitePixel
    mov r10, rax            ; r10 = white pixel
    pop r11                 ; r11 = black pixel

    ; Créer la fenêtre
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    xor edx, edx            ; x = 0
    xor ecx, ecx            ; y = 0
    mov r8d, MAX_X          ; width
    mov r9d, MAX_Y          ; height
    push 0                  ; border_width
    push r11                ; border color (black)
    push r10                ; background color (white)
    call XCreateSimpleWindow
    add rsp, 24
    mov qword[window], rax

    ; Select Input
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    mov rdx, 1
    call XSelectInput

    ; Map Window
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    call XMapWindow

    ; Create GC
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    xor edx, edx
    xor ecx, ecx
    call XCreateGC
    mov qword[gc], rax

    ; Petite pause pour que la fenêtre s'affiche
    mov rax, 35             ; sys_nanosleep
    lea rdi, [rsp-16]
    mov qword[rdi], 0       ; 0 secondes
    mov qword[rdi+8], 200000000  ; 200ms
    xor rsi, rsi
    syscall

    ; ===== DESSINER LES TRIANGLES =====
    call generer_n_triangles
    call dessiner_n_triangles

    ; Flush pour afficher
    mov rdi, qword[display_name]
    call XFlush

    ; Attendre (5 secondes)
    mov rax, 35
    lea rdi, [rsp-16]
    mov qword[rdi], 5
    mov qword[rdi+8], 0
    xor rsi, rsi
    syscall

    ; Fermer
    mov rdi, qword[display_name]
    call XCloseDisplay

    xor rdi, rdi
    call exit

.exit_error:
    mov rdi, 1
    call exit

; ===============================================
; Fonction: generer_nombre_aleatoire
; Entrée: rdi = valeur maximale
; Sortie: rax = nombre aléatoire entre 0 et rdi-1
; ===============================================
generer_nombre_aleatoire:
    push rbx
    push rcx
    push rdx
    
    mov rbx, rdi        ; Sauvegarder max
    
    ; Utiliser rdrand si disponible, sinon LCG
    rdrand rax
    jnc .use_lcg
    jmp .modulo
    
.use_lcg:
    ; LCG: seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF
    mov rax, qword[seed]
    mov rcx, 1103515245
    mul rcx
    add rax, 12345
    and rax, 0x7FFFFFFFFFFFFFFF
    mov qword[seed], rax
    
.modulo:
    ; Réduire à la plage [0, max[
    xor rdx, rdx
    div rbx
    mov rax, rdx
    
    pop rdx
    pop rcx
    pop rbx
    ret

; ===============================================
; Fonction: generer_triangle_aleatoire
; Génère les coordonnées de 3 points aléatoires
; Sortie: Les coordonnées sont dans les variables globales
; ===============================================
generer_triangle_aleatoire:
    push rbx
    
    ; Générer Ax
    mov rdi, MAX_X
    call generer_nombre_aleatoire
    mov qword[ax_coord], rax
    
    ; Générer Ay
    mov rdi, MAX_Y
    call generer_nombre_aleatoire
    mov qword[ay_coord], rax
    
    ; Générer Bx
    mov rdi, MAX_X
    call generer_nombre_aleatoire
    mov qword[bx_coord], rax
    
    ; Générer By
    mov rdi, MAX_Y
    call generer_nombre_aleatoire
    mov qword[by_coord], rax
    
    ; Générer Cx
    mov rdi, MAX_X
    call generer_nombre_aleatoire
    mov qword[cx_coord], rax
    
    ; Générer Cy
    mov rdi, MAX_Y
    call generer_nombre_aleatoire
    mov qword[cy_coord], rax
    
    pop rbx
    ret

; ===============================================
; Fonction: calculer_determinant
; Calcul du déterminant entre deux vecteurs partant du même point
; Entrée: 
;   rdi = x1 (point origine)
;   rsi = y1
;   rdx = x2 (premier point destination)
;   rcx = y2
;   r8  = x3 (second point destination)
;   r9  = y3
; Sortie: rax = déterminant (signé 64 bits)
; Formule: (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
; ===============================================
calculer_determinant:
    push rbx
    push r10
    push r11
    
    ; Calculer vecteur 1: (x2-x1, y2-y1)
    sub rdx, rdi        ; rdx = x2 - x1
    sub rcx, rsi        ; rcx = y2 - y1
    
    ; Calculer vecteur 2: (x3-x1, y3-y1)
    sub r8, rdi         ; r8 = x3 - x1
    sub r9, rsi         ; r9 = y3 - y1
    
    ; Premier produit: rdx * r9 = (x2-x1) * (y3-y1)
    mov rax, rdx
    imul rax, r9
    mov r10, rax        ; Sauvegarder le premier produit
    
    ; Second produit: r8 * rcx = (x3-x1) * (y2-y1)
    mov rax, r8
    imul rax, rcx
    
    ; Soustraire: premier - second
    mov rax, r10
    sub rax, rax        ; Bug ici! Devrait être sub r10, rax ou mov rax, r10 puis sub
    
    ; Correction:
    mov rax, r10
    sub rax, r8
    imul rcx            ; rax contient déjà le bon produit
    
    ; Refaire correctement:
    mov rax, rdx
    imul rax, r9        ; rax = (x2-x1) * (y3-y1)
    mov r10, rax
    
    mov rax, r8
    imul rax, rcx       ; rax = (x3-x1) * (y2-y1)
    
    sub r10, rax        ; r10 = premier - second
    mov rax, r10
    
    pop r11
    pop r10
    pop rbx
    ret

; ===============================================
; Fonction: est_triangle_direct
; Détermine si le triangle ABC est direct
; Sortie: rax = 1 si direct, 0 si indirect
; ===============================================
est_triangle_direct:
    push rbx
    
    ; Calculer le déterminant de BA et BC avec B comme origine
    mov rdi, qword[bx_coord]    ; x1 = bx
    mov rsi, qword[by_coord]    ; y1 = by
    mov rdx, qword[ax_coord]    ; x2 = ax
    mov rcx, qword[ay_coord]    ; y2 = ay
    mov r8,  qword[cx_coord]    ; x3 = cx
    mov r9,  qword[cy_coord]    ; y3 = cy
    
    call calculer_determinant
    
    ; Vérifier le signe du déterminant
    test rax, rax
    jns .positif        ; Si positif ou zéro
    
    ; Négatif = direct
    mov rax, 1
    jmp .fin
    
.positif:
    ; Positif = indirect
    xor rax, rax
    
.fin:
    pop rbx
    ret

; ===============================================
; Fonction: point_dans_triangle
; Vérifie si un point P est dans le triangle ABC
; Entrée: rdi = px, rsi = py
; Sortie: rax = 1 si dedans, 0 sinon
; ===============================================
point_dans_triangle:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    
    ; Variables locales
    ; [rbp-8]  = px
    ; [rbp-16] = py
    ; [rbp-24] = type triangle (1=direct, 0=indirect)
    ; [rbp-32] = det1
    ; [rbp-40] = det2
    ; [rbp-48] = det3
    
    mov qword[rbp-8], rdi   ; Sauvegarder px
    mov qword[rbp-16], rsi  ; Sauvegarder py
    
    ; Déterminer le type de triangle
    call est_triangle_direct
    mov qword[rbp-24], rax
    
    ; ===== Calcul det1: position par rapport à AB =====
    mov rdi, qword[ax_coord]
    mov rsi, qword[ay_coord]
    mov rdx, qword[bx_coord]
    mov rcx, qword[by_coord]
    mov r8,  qword[rbp-8]       ; px
    mov r9,  qword[rbp-16]      ; py
    call calculer_determinant
    mov qword[rbp-32], rax
    
    ; ===== Calcul det2: position par rapport à BC =====
    mov rdi, qword[bx_coord]
    mov rsi, qword[by_coord]
    mov rdx, qword[cx_coord]
    mov rcx, qword[cy_coord]
    mov r8,  qword[rbp-8]       ; px
    mov r9,  qword[rbp-16]      ; py
    call calculer_determinant
    mov qword[rbp-40], rax
    
    ; ===== Calcul det3: position par rapport à CA =====
    mov rdi, qword[cx_coord]
    mov rsi, qword[cy_coord]
    mov rdx, qword[ax_coord]
    mov rcx, qword[ay_coord]
    mov r8,  qword[rbp-8]       ; px
    mov r9,  qword[rbp-16]      ; py
    call calculer_determinant
    mov qword[rbp-48], rax
    
    ; Vérifier selon le type de triangle
    mov rax, qword[rbp-24]
    test rax, rax
    jz .triangle_indirect
    
.triangle_direct:
    ; Triangle direct : tous les déterminants <= 0 (à droite)
    mov rax, qword[rbp-32]
    test rax, rax
    jg .dehors
    
    mov rax, qword[rbp-40]
    test rax, rax
    jg .dehors
    
    mov rax, qword[rbp-48]
    test rax, rax
    jg .dehors
    
    jmp .dedans
    
.triangle_indirect:
    ; Triangle indirect : tous les déterminants >= 0 (à gauche)
    mov rax, qword[rbp-32]
    test rax, rax
    jl .dehors
    
    mov rax, qword[rbp-40]
    test rax, rax
    jl .dehors
    
    mov rax, qword[rbp-48]
    test rax, rax
    jl .dehors
    
.dedans:
    mov rax, 1
    jmp .fin
    
.dehors:
    xor rax, rax
    
.fin:
    leave
    ret

; ===============================================
; Fonction: min_de_trois
; Trouve le minimum de trois valeurs
; Entrée: rdi, rsi, rdx = trois valeurs
; Sortie: rax = minimum
; ===============================================
min_de_trois:
    mov rax, rdi
    cmp rax, rsi
    cmovg rax, rsi      ; if (rax > rsi) rax = rsi
    cmp rax, rdx
    cmovg rax, rdx      ; if (rax > rdx) rax = rdx
    ret

; ===============================================
; Fonction: max_de_trois
; Trouve le maximum de trois valeurs
; Entrée: rdi, rsi, rdx = trois valeurs
; Sortie: rax = maximum
; ===============================================
max_de_trois:
    mov rax, rdi
    cmp rax, rsi
    cmovl rax, rsi      ; if (rax < rsi) rax = rsi
    cmp rax, rdx
    cmovl rax, rdx      ; if (rax < rdx) rax = rdx
    ret

; ===============================================
; Fonction: remplir_triangle
; Remplit le triangle avec la couleur donnée
; Entrée: edi = couleur (RGB 0x00RRGGBB)
; ===============================================
remplir_triangle:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    
    ; Variables locales
    ; [rbp-8]  = couleur
    ; [rbp-16] = min_x
    ; [rbp-24] = max_x
    ; [rbp-32] = min_y
    ; [rbp-40] = max_y
    ; [rbp-48] = y courant
    
    mov dword[rbp-8], edi   ; Sauvegarder la couleur
    
    ; Calculer min_x
    mov rdi, qword[ax_coord]
    mov rsi, qword[bx_coord]
    mov rdx, qword[cx_coord]
    call min_de_trois
    mov qword[rbp-16], rax
    
    ; Calculer max_x
    mov rdi, qword[ax_coord]
    mov rsi, qword[bx_coord]
    mov rdx, qword[cx_coord]
    call max_de_trois
    mov qword[rbp-24], rax
    
    ; Calculer min_y
    mov rdi, qword[ay_coord]
    mov rsi, qword[by_coord]
    mov rdx, qword[cy_coord]
    call min_de_trois
    mov qword[rbp-32], rax
    
    ; Calculer max_y
    mov rdi, qword[ay_coord]
    mov rsi, qword[by_coord]
    mov rdx, qword[cy_coord]
    call max_de_trois
    mov qword[rbp-40], rax
    
    ; Boucle sur Y
    mov rax, qword[rbp-32]
    mov qword[rbp-48], rax
    
.loop_y:
    mov rax, qword[rbp-48]
    cmp rax, qword[rbp-40]
    jg .fin_loop_y
    
    ; Boucle sur X
    mov r12, qword[rbp-16]  ; x = min_x
    
.loop_x:
    cmp r12, qword[rbp-24]
    jg .fin_loop_x
    
    ; Vérifier si le point (r12, [rbp-48]) est dans le triangle
    mov rdi, r12
    mov rsi, qword[rbp-48]
    
    ; Sauvegarder r12
    push r12
    call point_dans_triangle
    pop r12
    
    test rax, rax
    jz .pas_dessiner
    
    ; Dessiner le pixel
    ; XDrawPoint(display, window, gc, x, y)
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    mov rdx, qword[gc]
    mov ecx, r12d           ; x
    mov r8d, dword[rbp-48]  ; y
    
    push r12
    call XDrawPoint
    pop r12
    
.pas_dessiner:
    inc r12
    jmp .loop_x
    
.fin_loop_x:
    inc qword[rbp-48]
    jmp .loop_y
    
.fin_loop_y:
    leave
    ret

; ===============================================
; Fonction: dessiner_contour_triangle
; Dessine le contour du triangle
; Entrée: edi = couleur
; ===============================================
dessiner_contour_triangle:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    
    mov dword[rbp-4], edi   ; Sauvegarder la couleur
    
    ; Définir la couleur
    mov rdi, qword[display_name]
    mov rsi, qword[gc]
    mov edx, dword[rbp-4]
    call XSetForeground
    
    ; Ligne AB
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    mov rdx, qword[gc]
    mov ecx, dword[ax_coord]
    mov r8d, dword[ay_coord]
    mov r9d, dword[bx_coord]
    push qword[by_coord]
    call XDrawLine
    add rsp, 8
    
    ; Ligne BC
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    mov rdx, qword[gc]
    mov ecx, dword[bx_coord]
    mov r8d, dword[by_coord]
    mov r9d, dword[cx_coord]
    push qword[cy_coord]
    call XDrawLine
    add rsp, 8
    
    ; Ligne CA
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    mov rdx, qword[gc]
    mov ecx, dword[cx_coord]
    mov r8d, dword[cy_coord]
    mov r9d, dword[ax_coord]
    push qword[ay_coord]
    call XDrawLine
    add rsp, 8
    
    leave
    ret

; ===============================================
; Fonction: generer_couleur_aleatoire
; Génère une couleur RGB aléatoire
; Sortie: eax = couleur (0x00RRGGBB)
; ===============================================
generer_couleur_aleatoire:
    push rbx
    
    ; Rouge
    mov rdi, 256
    call generer_nombre_aleatoire
    shl rax, 16
    mov rbx, rax
    
    ; Vert
    mov rdi, 256
    call generer_nombre_aleatoire
    shl rax, 8
    or rbx, rax
    
    ; Bleu
    mov rdi, 256
    call generer_nombre_aleatoire
    or rbx, rax
    
    mov rax, rbx
    pop rbx
    ret

; ===============================================
; Fonction: generer_n_triangles
; Génère N triangles aléatoires
; ===============================================
generer_n_triangles:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    
    xor r12, r12            ; compteur = 0
    lea r13, [triangles]    ; Pointeur vers le tableau
    
.loop:
    cmp r12, qword[nb_triangles_a_dessiner]
    jge .fin
    
    ; Générer 6 coordonnées
    mov rdi, MAX_X
    call generer_nombre_aleatoire
    mov qword[r13], rax     ; ax
    add r13, 8
    
    mov rdi, MAX_Y
    call generer_nombre_aleatoire
    mov qword[r13], rax     ; ay
    add r13, 8
    
    mov rdi, MAX_X
    call generer_nombre_aleatoire
    mov qword[r13], rax     ; bx
    add r13, 8
    
    mov rdi, MAX_Y
    call generer_nombre_aleatoire
    mov qword[r13], rax     ; by
    add r13, 8
    
    mov rdi, MAX_X
    call generer_nombre_aleatoire
    mov qword[r13], rax     ; cx
    add r13, 8
    
    mov rdi, MAX_Y
    call generer_nombre_aleatoire
    mov qword[r13], rax     ; cy
    add r13, 8
    
    ; Générer une couleur
    call generer_couleur_aleatoire
    lea rbx, [couleurs]
    mov dword[rbx + r12*4], eax
    
    inc r12
    jmp .loop
    
.fin:
    pop r13
    pop r12
    pop rbx
    leave
    ret

; ===============================================
; Fonction: dessiner_n_triangles
; Dessine tous les triangles générés
; ===============================================
dessiner_n_triangles:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    
    xor r12, r12            ; compteur = 0
    lea r13, [triangles]    ; Pointeur vers le tableau
    
.loop:
    cmp r12, qword[nb_triangles_a_dessiner]
    jge .fin
    
    ; Charger le triangle courant dans les variables globales
    mov rax, qword[r13]
    mov qword[ax_coord], rax
    mov rax, qword[r13+8]
    mov qword[ay_coord], rax
    mov rax, qword[r13+16]
    mov qword[bx_coord], rax
    mov rax, qword[r13+24]
    mov qword[by_coord], rax
    mov rax, qword[r13+32]
    mov qword[cx_coord], rax
    mov rax, qword[r13+40]
    mov qword[cy_coord], rax
    
    ; Charger la couleur
    lea rbx, [couleurs]
    mov edi, dword[rbx + r12*4]
    
    ; Définir la couleur pour le remplissage
    push rdi
    mov rdi, qword[display_name]
    mov rsi, qword[gc]
    mov edx, dword[rbx + r12*4]
    call XSetForeground
    pop rdi
    
    ; Remplir le triangle
    push r12
    push r13
    call remplir_triangle
    pop r13
    pop r12
    
    ; Dessiner le contour en noir (couleur 0x000000)
    push r12
    push r13
    mov edi, 0x000000
    call dessiner_contour_triangle
    pop r13
    pop r12
    
    ; Flush après chaque triangle pour voir le dessin progresser
    push r12
    push r13
    mov rdi, qword[display_name]
    call XFlush
    pop r13
    pop r12
    
    ; Passer au triangle suivant
    add r13, 48             ; 6 qwords de 8 octets
    inc r12
    jmp .loop
    
.fin:
    pop r13
    pop r12
    pop rbx
    leave
    ret
