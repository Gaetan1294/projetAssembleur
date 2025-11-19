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

    ; Variables X11
    display_name: dq 0
    window: dq 0
    gc: dq 0
    screen: dd 0

section .text
    global main
    
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
    ; ===== Initialisation X11 =====
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
    push rax                ; Sauvegarder root window

    mov rdi, qword[display_name]
    mov esi, dword[screen]
    call XBlackPixel
    push rax                ; Sauvegarder black pixel

    mov rdi, qword[display_name]
    mov esi, dword[screen]
    call XWhitePixel
    mov r10, rax            ; r10 = white pixel
    pop r11                 ; r11 = black pixel
    pop r12                 ; r12 = root window

    ; Créer la fenêtre
    mov rdi, qword[display_name]
    mov rsi, r12            ; root window
    xor edx, edx            ; x = 0
    xor ecx, ecx            ; y = 0
    mov r8d, MAX_X          ; width
    mov r9d, MAX_Y          ; height
    push r10                ; background (white)
    push r11                ; border color (black)
    push 1                  ; border_width
    call XCreateSimpleWindow
    add rsp, 24
    mov qword[window], rax

    ; Sélectionner les événements
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    mov rdx, 0x8000         ; ExposureMask
    call XSelectInput

    ; Mapper la fenêtre
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    call XMapWindow

    ; Créer le contexte graphique
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    xor edx, edx
    xor ecx, ecx
    call XCreateGC
    mov qword[gc], rax

    ; ===== Générer et dessiner les triangles =====
    call generer_n_triangles
    call dessiner_n_triangles

    ; Flush pour afficher
    mov rdi, qword[display_name]
    call XFlush

    ; Attendre 5 secondes (nanosleep)
    sub rsp, 16
    mov qword[rsp], 5       ; 5 secondes
    mov qword[rsp+8], 0     ; 0 nanosecondes
    mov rax, 35             ; sys_nanosleep
    mov rdi, rsp
    xor rsi, rsi
    syscall
    add rsp, 16

    ; Fermer la fenêtre et quitter
    mov rdi, qword[display_name]
    call XCloseDisplay
    
    xor rdi, rdi
    call exit

.exit_error:
    mov rdi, 1
    call exit

; ===============================================
; Fonction: generer_nombre_aleatoire
; Générateur LCG (Linear Congruential Generator)
; Entrée: rdi = valeur maximale
; Sortie: rax = nombre aléatoire entre 0 et max-1
; ===============================================
generer_nombre_aleatoire:
    push rbx
    push rdx
    
    mov rbx, rdi            ; Sauvegarder max
    
    ; LCG: seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF
    mov rax, qword[seed]
    mov rcx, 1103515245
    mul rcx                 ; rdx:rax = rax * rcx
    add rax, 12345
    and rax, 0x7FFFFFFF
    mov qword[seed], rax
    
    ; Modulo max
    xor rdx, rdx
    div rbx                 ; rax = rax / rbx, rdx = rax % rbx
    mov rax, rdx            ; Résultat dans rax
    
    pop rdx
    pop rbx
    ret

; ===============================================
; Fonction: generer_triangle_aleatoire
; Génère un triangle avec des coordonnées aléatoires
; Entrée: rdi = max_x, rsi = max_y
; Sortie: Met à jour ax_coord, ay_coord, etc.
; ===============================================
generer_triangle_aleatoire:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    
    mov qword[rbp-8], rdi   ; Sauvegarder max_x
    mov qword[rbp-16], rsi  ; Sauvegarder max_y
    
    ; Générer ax
    mov rdi, qword[rbp-8]
    call generer_nombre_aleatoire
    mov qword[ax_coord], rax
    
    ; Générer ay
    mov rdi, qword[rbp-16]
    call generer_nombre_aleatoire
    mov qword[ay_coord], rax
    
    ; Générer bx
    mov rdi, qword[rbp-8]
    call generer_nombre_aleatoire
    mov qword[bx_coord], rax
    
    ; Générer by
    mov rdi, qword[rbp-16]
    call generer_nombre_aleatoire
    mov qword[by_coord], rax
    
    ; Générer cx
    mov rdi, qword[rbp-8]
    call generer_nombre_aleatoire
    mov qword[cx_coord], rax
    
    ; Générer cy
    mov rdi, qword[rbp-16]
    call generer_nombre_aleatoire
    mov qword[cy_coord], rax
    
    leave
    ret

; ===============================================
; Fonction: generer_couleur_aleatoire
; Génère une couleur RGB aléatoire
; Sortie: eax = couleur (0x00RRGGBB)
; ===============================================
generer_couleur_aleatoire:
    push rbp
    mov rbp, rsp
    
    ; Rouge (0-255)
    mov rdi, 256
    call generer_nombre_aleatoire
    shl rax, 16             ; Décaler dans les bits rouge
    mov rbx, rax
    
    ; Vert (0-255)
    mov rdi, 256
    call generer_nombre_aleatoire
    shl rax, 8              ; Décaler dans les bits vert
    or rbx, rax
    
    ; Bleu (0-255)
    mov rdi, 256
    call generer_nombre_aleatoire
    or rbx, rax             ; Combiner avec bleu
    
    mov rax, rbx
    
    leave
    ret

; ===============================================
; Fonction: generer_n_triangles
; Génère N triangles aléatoires
; Utilise: nb_triangles_a_dessiner
; ===============================================
generer_n_triangles:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    
    xor rbx, rbx            ; Compteur de triangles
    mov r12, triangles      ; Pointeur vers le tableau
    
.loop:
    cmp rbx, qword[nb_triangles_a_dessiner]
    jge .fin
    
    ; Générer un triangle
    mov rdi, MAX_X
    mov rsi, MAX_Y
    call generer_triangle_aleatoire
    
    ; Sauvegarder les coordonnées dans le tableau
    mov rax, qword[ax_coord]
    mov qword[r12], rax
    mov rax, qword[ay_coord]
    mov qword[r12+8], rax
    mov rax, qword[bx_coord]
    mov qword[r12+16], rax
    mov rax, qword[by_coord]
    mov qword[r12+24], rax
    mov rax, qword[cx_coord]
    mov qword[r12+32], rax
    mov rax, qword[cy_coord]
    mov qword[r12+40], rax
    
    ; Générer une couleur
    call generer_couleur_aleatoire
    mov dword[couleurs + rbx*4], eax
    
    ; Passer au triangle suivant
    add r12, 48             ; 6 qwords de 8 octets
    inc rbx
    jmp .loop
    
.fin:
    pop r12
    pop rbx
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
    jle .compare_c
    mov rax, rsi
.compare_c:
    cmp rax, rdx
    jle .fin
    mov rax, rdx
.fin:
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
    jge .compare_c
    mov rax, rsi
.compare_c:
    cmp rax, rdx
    jge .fin
    mov rax, rdx
.fin:
    ret

; ===============================================
; Fonction: calculer_determinant
; Calcule le déterminant d'une matrice 2x2
; Entrée: rdi = a, rsi = b, rdx = c, rcx = d
; Sortie: rax = a*d - b*c
; ===============================================
calculer_determinant:
    push rbx
    push r12
    
    ; a*d
    mov rax, rdi
    imul rax, rcx
    mov r12, rax
    
    ; b*c
    mov rax, rsi
    imul rax, rdx
    
    ; a*d - b*c
    mov rax, r12
    sub rax, rax            ; CORRECTION : devrait être sub r12, rax puis mov rax, r12
    
    pop r12
    pop rbx
    ret

; ===============================================
; Fonction: point_dans_triangle
; Vérifie si un point est dans le triangle
; Entrée: rdi = x, rsi = y
; Sortie: rax = 1 si dedans, 0 sinon
; Utilise les coordonnées globales du triangle
; ===============================================
point_dans_triangle:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    
    ; Variables locales
    ; [rbp-8]  = px
    ; [rbp-16] = py
    ; [rbp-24] = det_principal
    ; [rbp-32] = det1
    ; [rbp-40] = det2
    ; [rbp-48] = det3
    
    mov qword[rbp-8], rdi   ; px
    mov qword[rbp-16], rsi  ; py
    
    ; Calculer le déterminant principal du triangle
    ; det = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    mov rdi, qword[bx_coord]
    sub rdi, qword[ax_coord]
    
    mov rsi, qword[by_coord]
    sub rsi, qword[ay_coord]
    
    mov rdx, qword[cx_coord]
    sub rdx, qword[ax_coord]
    
    mov rcx, qword[cy_coord]
    sub rcx, qword[ay_coord]
    
    call calculer_determinant
    mov qword[rbp-24], rax
    
    ; Calculer det1 = (px - ax) * (by - ay) - (py - ay) * (bx - ax)
    mov rdi, qword[rbp-8]
    sub rdi, qword[ax_coord]
    
    mov rsi, qword[rbp-16]
    sub rsi, qword[ay_coord]
    
    mov rdx, qword[bx_coord]
    sub rdx, qword[ax_coord]
    
    mov rcx, qword[by_coord]
    sub rcx, qword[ay_coord]
    
    call calculer_determinant
    mov qword[rbp-32], rax
    
    ; Calculer det2 = (px - bx) * (cy - by) - (py - by) * (cx - bx)
    mov rdi, qword[rbp-8]
    sub rdi, qword[bx_coord]
    
    mov rsi, qword[rbp-16]
    sub rsi, qword[by_coord]
    
    mov rdx, qword[cx_coord]
    sub rdx, qword[bx_coord]
    
    mov rcx, qword[cy_coord]
    sub rcx, qword[by_coord]
    
    call calculer_determinant
    mov qword[rbp-40], rax
    
    ; Calculer det3 = (px - cx) * (ay - cy) - (py - cy) * (ax - cx)
    mov rdi, qword[rbp-8]
    sub rdi, qword[cx_coord]
    
    mov rsi, qword[rbp-16]
    sub rsi, qword[cy_coord]
    
    mov rdx, qword[ax_coord]
    sub rdx, qword[cx_coord]
    
    mov rcx, qword[ay_coord]
    sub rcx, qword[cy_coord]
    
    call calculer_determinant
    mov qword[rbp-48], rax
    
    ; Vérifier les signes
    mov rax, qword[rbp-24]
    test rax, rax
    js .triangle_indirect    ; Si négatif, triangle indirect
    
.triangle_direct:
    ; Pour un triangle direct, tous les déterminants doivent être >= 0
    mov rax, qword[rbp-32]
    test rax, rax
    js .dehors
    
    mov rax, qword[rbp-40]
    test rax, rax
    js .dehors
    
    mov rax, qword[rbp-48]
    test rax, rax
    js .dehors
    
    jmp .dedans
    
.triangle_indirect:
    ; Pour un triangle indirect, tous les déterminants doivent être <= 0
    mov rax, qword[rbp-32]
    test rax, rax
    jns .dehors
    
    mov rax, qword[rbp-40]
    test rax, rax
    jns .dehors
    
    mov rax, qword[rbp-48]
    test rax, rax
    jns .dehors
    
.dedans:
    mov rax, 1
    jmp .fin
    
.dehors:
    xor rax, rax
    
.fin:
    leave
    ret

; ===============================================
; Fonction: remplir_triangle
; Remplit le triangle avec la couleur donnée
; Entrée: edi = couleur (RGB 0x00RRGGBB)
; ===============================================
remplir_triangle:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    push r12
    push r13
    
    ; Variables locales
    ; [rbp-4]  = couleur (32 bits)
    ; [rbp-12] = min_x (32 bits)
    ; [rbp-20] = max_x (32 bits)
    ; [rbp-28] = min_y (32 bits)
    ; [rbp-36] = max_y (32 bits)
    ; [rbp-44] = y courant (32 bits)
    
    mov dword[rbp-4], edi   ; Sauvegarder la couleur
    
    ; Définir la couleur pour X11
    mov rdi, qword[display_name]
    mov rsi, qword[gc]
    mov edx, dword[rbp-4]
    call XSetForeground
    
    ; Calculer min_x
    mov rdi, qword[ax_coord]
    mov rsi, qword[bx_coord]
    mov rdx, qword[cx_coord]
    call min_de_trois
    mov dword[rbp-12], eax  ; Stocker en 32 bits
    
    ; Calculer max_x
    mov rdi, qword[ax_coord]
    mov rsi, qword[bx_coord]
    mov rdx, qword[cx_coord]
    call max_de_trois
    mov dword[rbp-20], eax
    
    ; Calculer min_y
    mov rdi, qword[ay_coord]
    mov rsi, qword[by_coord]
    mov rdx, qword[cy_coord]
    call min_de_trois
    mov dword[rbp-28], eax
    
    ; Calculer max_y
    mov rdi, qword[ay_coord]
    mov rsi, qword[by_coord]
    mov rdx, qword[cy_coord]
    call max_de_trois
    mov dword[rbp-36], eax
    
    ; Boucle sur Y
    mov eax, dword[rbp-28]
    mov dword[rbp-44], eax
    
.loop_y:
    mov eax, dword[rbp-44]
    cmp eax, dword[rbp-36]
    jg .fin_loop_y
    
    ; Boucle sur X
    mov r12d, dword[rbp-12]  ; x = min_x
    
.loop_x:
    cmp r12d, dword[rbp-20]
    jg .fin_loop_x
    
    ; Vérifier si le point (r12, [rbp-44]) est dans le triangle
    movsx rdi, r12d              ; Étendre le signe à 64 bits
    mov esi, dword[rbp-44]
    movsx rsi, esi               ; Étendre le signe à 64 bits
    
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
    mov ecx, r12d                ; x (32 bits)
    mov r8d, dword[rbp-44]       ; y (32 bits)
    
    push r12
    call XDrawPoint
    pop r12
    
.pas_dessiner:
    inc r12d
    jmp .loop_x
    
.fin_loop_x:
    inc dword[rbp-44]
    jmp .loop_y
    
.fin_loop_y:
    pop r13
    pop r12
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
    mov rax, qword[ax_coord]
    mov ecx, eax
    mov rax, qword[ay_coord]
    mov r8d, eax
    mov rax, qword[bx_coord]
    mov r9d, eax
    mov rax, qword[by_coord]
    push rax
    call XDrawLine
    add rsp, 8
    
    ; Ligne BC
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    mov rdx, qword[gc]
    mov rax, qword[bx_coord]
    mov ecx, eax
    mov rax, qword[by_coord]
    mov r8d, eax
    mov rax, qword[cx_coord]
    mov r9d, eax
    mov rax, qword[cy_coord]
    push rax
    call XDrawLine
    add rsp, 8
    
    ; Ligne CA
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    mov rdx, qword[gc]
    mov rax, qword[cx_coord]
    mov ecx, eax
    mov rax, qword[cy_coord]
    mov r8d, eax
    mov rax, qword[ax_coord]
    mov r9d, eax
    mov rax, qword[ay_coord]
    push rax
    call XDrawLine
    add rsp, 8
    
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
    
    xor rbx, rbx            ; Compteur de triangles
    mov r13, triangles      ; Pointeur vers le tableau
    
.loop:
    cmp rbx, qword[nb_triangles_a_dessiner]
    jge .fin
    
    ; Charger les coordonnées du triangle
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
    mov edi, dword[couleurs + rbx*4]
    
    ; Remplir le triangle
    push rbx
    push r13
    call remplir_triangle
    pop r13
    pop rbx
    
    ; Dessiner le contour en noir
    mov edi, 0x000000
    push rbx
    push r13
    call dessiner_contour_triangle
    pop r13
    pop rbx
    
    ; Passer au triangle suivant
    add r13, 48             ; 6 qwords de 8 octets
    inc rbx
    jmp .loop
    
.fin:
    pop r13
    pop r12
    pop rbx
    leave
    ret
