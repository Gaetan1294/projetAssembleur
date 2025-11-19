section .data
    ; Constantes pour les limites de la fenêtre
    MAX_X equ 400
    MAX_Y equ 400
    
    ; Coordonnées des points du triangle
    ax: dd 0
    ay: dd 0
    bx: dd 0
    by: dd 0
    cx: dd 0
    cy: dd 0

section .text

; ===============================================
; Fonction: generer_nombre_aleatoire
; Entrée: rdi = valeur maximale
; Sortie: rax = nombre aléatoire entre 0 et rdi-1
; ===============================================
generer_nombre_aleatoire:
    push rbx
    push rcx
    
    mov rbx, rdi        ; Sauvegarder la valeur max
    
.retry:
    rdrand ax           ; Générer nombre aléatoire (16 bits)
    jnc .retry          ; Si CF=0, recommencer
    
    ; Réduire le nombre à la plage [0, max[
    xor rdx, rdx
    movzx rax, ax       ; Étendre à 64 bits
    div rbx             ; rax = rax / rbx, reste dans rdx
    mov rax, rdx        ; Le reste est notre nombre aléatoire
    
    pop rcx
    pop rbx
    ret

; ===============================================
; Fonction: generer_triangle_aleatoire
; Génère les coordonnées de 3 points aléatoires
; ===============================================
generer_triangle_aleatoire:
    push rbx
    push r12
    
    ; Générer Ax
    mov rdi, MAX_X
    call generer_nombre_aleatoire
    mov dword[ax], eax
    
    ; Générer Ay
    mov rdi, MAX_Y
    call generer_nombre_aleatoire
    mov dword[ay], eax
    
    ; Générer Bx
    mov rdi, MAX_X
    call generer_nombre_aleatoire
    mov dword[bx], eax
    
    ; Générer By
    mov rdi, MAX_Y
    call generer_nombre_aleatoire
    mov dword[by], eax
    
    ; Générer Cx
    mov rdi, MAX_X
    call generer_nombre_aleatoire
    mov dword[cx], eax
    
    ; Générer Cy
    mov rdi, MAX_Y
    call generer_nombre_aleatoire
    mov dword[cy], eax
    
    pop r12
    pop rbx
    ret

; ===============================================
; Fonction: dessiner_contour_triangle
; Dessine les 3 côtés du triangle
; ===============================================
dessiner_contour_triangle:
    push rbp
    mov rbp, rsp
    
    ; Sauvegarder les registres
    push rbx
    push r12
    push r13
    
    ; Dessiner AB
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    mov rdx, qword[gc]
    mov ecx, dword[ax]
    mov r8d, dword[ay]
    mov r9d, dword[bx]
    push qword[by]
    call XDrawLine
    add rsp, 8
    
    ; Dessiner BC
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    mov rdx, qword[gc]
    mov ecx, dword[bx]
    mov r8d, dword[by]
    mov r9d, dword[cx]
    push qword[cy]
    call XDrawLine
    add rsp, 8
    
    ; Dessiner CA
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    mov rdx, qword[gc]
    mov ecx, dword[cx]
    mov r8d, dword[cy]
    mov r9d, dword[ax]
    push qword[ay]
    call XDrawLine
    add rsp, 8
    
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret


; ===============================================
; Fonction: calculer_determinant
; Calcul du déterminant de deux vecteurs
; Entrée: 
;   rdi = x1 (premier point du vecteur 1)
;   rsi = y1
;   rdx = x2 (deuxième point du vecteur 1)
;   rcx = y2
;   r8  = x3 (deuxième point du vecteur 2)
;   r9  = y3
; Sortie: rax = déterminant (signé)
; Formule: (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
; ===============================================
calculer_determinant:
    push rbx
    push r12
    push r13
    push r14
    
    ; Calculer le vecteur 1: (x2-x1, y2-y1)
    mov r10, rdx
    sub r10, rdi        ; r10 = x2 - x1 (x du vecteur 1)
    
    mov r11, rcx
    sub r11, rsi        ; r11 = y2 - y1 (y du vecteur 1)
    
    ; Calculer le vecteur 2: (x3-x1, y3-y1)
    mov r12, r8
    sub r12, rdi        ; r12 = x3 - x1 (x du vecteur 2)
    
    mov r13, r9
    sub r13, rsi        ; r13 = y3 - y1 (y du vecteur 2)
    
    ; Déterminant = (x_vec1 * y_vec2) - (x_vec2 * y_vec1)
    mov rax, r10
    imul rax, r13       ; rax = (x2-x1) * (y3-y1)
    
    mov rbx, r12
    imul rbx, r11       ; rbx = (x3-x1) * (y2-y1)
    
    sub rax, rbx        ; rax = déterminant
    
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

; ===============================================
; Fonction: est_triangle_direct
; Détermine si le triangle ABC est direct
; Sortie: rax = 1 si direct, 0 si indirect
; ===============================================
est_triangle_direct:
    push rbp
    mov rbp, rsp
    
    ; Calculer déterminant de BA et BC
    ; Point B est la base
    movsxd rdi, dword[bx]
    movsxd rsi, dword[by]
    movsxd rdx, dword[ax]
    movsxd rcx, dword[ay]
    movsxd r8, dword[cx]
    movsxd r9, dword[cy]
    
    call calculer_determinant
    
    ; Si déterminant < 0 : direct (rax = 1)
    ; Si déterminant > 0 : indirect (rax = 0)
    xor rbx, rbx
    test rax, rax
    sets bl             ; bl = 1 si négatif
    mov rax, rbx
    
    pop rbp
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
    sub rsp, 32         ; Espace pour variables locales
    
    ; Sauvegarder px et py
    mov [rbp-8], rdi    ; px
    mov [rbp-16], rsi   ; py
    
    ; Déterminer si triangle est direct
    call est_triangle_direct
    mov [rbp-24], rax   ; Sauvegarder (1=direct, 0=indirect)
    
    ; ===== Vérifier position par rapport à AB =====
    movsxd rdi, dword[ax]
    movsxd rsi, dword[ay]
    movsxd rdx, dword[bx]
    movsxd rcx, dword[by]
    mov r8, [rbp-8]     ; px
    mov r9, [rbp-16]    ; py
    
    call calculer_determinant
    mov [rbp-32], rax   ; Sauvegarder det1
    
    ; ===== Vérifier position par rapport à BC =====
    movsxd rdi, dword[bx]
    movsxd rsi, dword[by]
    movsxd rdx, dword[cx]
    movsxd rcx, dword[cy]
    mov r8, [rbp-8]     ; px
    mov r9, [rbp-16]    ; py
    
    call calculer_determinant
    mov rbx, rax        ; det2 dans rbx
    
    ; ===== Vérifier position par rapport à CA =====
    movsxd rdi, dword[cx]
    movsxd rsi, dword[cy]
    movsxd rdx, dword[ax]
    movsxd rcx, dword[ay]
    mov r8, [rbp-8]     ; px
    mov r9, [rbp-16]    ; py
    
    call calculer_determinant
    mov rcx, rax        ; det3 dans rcx
    
    ; Récupérer det1
    mov rdx, [rbp-32]
    
    ; Vérifier selon le type de triangle
    mov r10, [rbp-24]   ; type (1=direct, 0=indirect)
    test r10, r10
    jz .triangle_indirect
    
.triangle_direct:
    ; Triangle direct : tous les déterminants doivent être >= 0 (à droite)
    test rdx, rdx
    js .dehors          ; det1 < 0
    test rbx, rbx
    js .dehors          ; det2 < 0
    test rcx, rcx
    js .dehors          ; det3 < 0
    jmp .dedans
    
.triangle_indirect:
    ; Triangle indirect : tous les déterminants doivent être <= 0 (à gauche)
    test rdx, rdx
    jns .dehors         ; det1 > 0
    test rbx, rbx
    jns .dehors         ; det2 > 0
    test rcx, rcx
    jns .dehors         ; det3 > 0
    
.dedans:
    mov rax, 1
    jmp .fin
    
.dehors:
    xor rax, rax
    
.fin:
    add rsp, 32
    pop rbp
    ret

; ===============================================
; Fonction: remplir_triangle
; Remplit le triangle avec la couleur courante
; ===============================================
remplir_triangle:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    
    ; Trouver la bounding box (min/max x et y)
    mov eax, dword[ax]
    mov ebx, dword[bx]
    mov ecx, dword[cx]
    
    ; min_x = min(ax, bx, cx)
    cmp eax, ebx
    cmovg eax, ebx
    cmp eax, ecx
    cmovg eax, ecx
    mov r12d, eax       ; r12d = min_x
    
    ; max_x = max(ax, bx, cx)
    mov eax, dword[ax]
    mov ebx, dword[bx]
    mov ecx, dword[cx]
    cmp eax, ebx
    cmovl eax, ebx
    cmp eax, ecx
    cmovl eax, ecx
    mov r13d, eax       ; r13d = max_x
    
    ; min_y = min(ay, by, cy)
    mov eax, dword[ay]
    mov ebx, dword[by]
    mov ecx, dword[cy]
    cmp eax, ebx
    cmovg eax, ebx
    cmp eax, ecx
    cmovg eax, ecx
    mov r14d, eax       ; r14d = min_y
    
    ; max_y = max(ay, by, cy)
    mov eax, dword[ay]
    mov ebx, dword[by]
    mov ecx, dword[cy]
    cmp eax, ebx
    cmovl eax, ebx
    cmp eax, ecx
    cmovl eax, ecx
    mov r15d, eax       ; r15d = max_y
    
    ; Parcourir tous les points de la bounding box
    mov ebx, r14d       ; y = min_y
    
.loop_y:
    cmp ebx, r15d
    jg .fin_loop_y
    
    mov ecx, r12d       ; x = min_x
    
.loop_x:
    cmp ecx, r13d
    jg .fin_loop_x
    
    ; Vérifier si le point (ecx, ebx) est dans le triangle
    movsxd rdi, ecx
    movsxd rsi, ebx
    
    push rbx
    push rcx
    push r12
    push r13
    push r14
    push r15
    
    call point_dans_triangle
    
    pop r15
    pop r14
    pop r13
    pop r12
    pop rcx
    pop rbx
    
    test rax, rax
    jz .pas_dessiner
    
    ; Dessiner le point
    push rbx
    push rcx
    push r12
    push r13
    push r14
    push r15
    
    mov rdi, qword[display_name]
    mov rsi, qword[window]
    mov rdx, qword[gc]
    ; ecx et ebx contiennent déjà x et y
    mov r8d, ebx
    call XDrawPoint
    
    pop r15
    pop r14
    pop r13
    pop r12
    pop rcx
    pop rbx
    
.pas_dessiner:
    inc ecx
    jmp .loop_x
    
.fin_loop_x:
    inc ebx
    jmp .loop_y
    
.fin_loop_y:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
dessin:
    ; Générer un triangle aléatoire
    call generer_triangle_aleatoire
    
    ; Choisir une couleur
    mov rdi, qword[display_name]
    mov rsi, qword[gc]
    mov edx, 0xFF0000   ; Rouge
    call XSetForeground
    
    ; Remplir le triangle
    call remplir_triangle
    
    ; Dessiner le contour en noir
    mov rdi, qword[display_name]
    mov rsi, qword[gc]
    mov edx, 0x000000   ; Noir
    call XSetForeground
    
    call dessiner_contour_triangle
    
    jmp flush
%define NB_TRIANGLES 5

section .data
    ; Tableau de triangles (6 coordonnées par triangle)
    triangles: times (NB_TRIANGLES * 6) dd 0
    ; Tableau de couleurs
    couleurs: dd 0xFF0000, 0x00FF00, 0x0000FF, 0xFFFF00, 0xFF00FF

section .text

generer_n_triangles:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    
    xor r12, r12        ; compteur = 0
    
.loop:
    cmp r12, NB_TRIANGLES
    jge .fin
    
    ; Calculer l'offset dans le tableau
    mov rax, r12
    imul rax, 6
    imul rax, 4         ; * sizeof(dword)
    lea rbx, [triangles + rax]
    
    ; Générer 6 coordonnées
    mov rdi, MAX_X
    call generer_nombre_aleatoire
    mov [rbx], eax      ; ax
    
    mov rdi, MAX_Y
    call generer_nombre_aleatoire
    mov [rbx+4], eax    ; ay
    
    mov rdi, MAX_X
    call generer_nombre_aleatoire
    mov [rbx+8], eax    ; bx
    
    mov rdi, MAX_Y
    call generer_nombre_aleatoire
    mov [rbx+12], eax   ; by
    
    mov rdi, MAX_X
    call generer_nombre_aleatoire
    mov [rbx+16], eax   ; cx
    
    mov rdi, MAX_Y
    call generer_nombre_aleatoire
    mov [rbx+20], eax   ; cy
    
    inc r12
    jmp .loop
    
.fin:
    pop r12
    pop rbx
    pop rbp
    ret

dessiner_n_triangles:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    
    xor r12, r12
    
.loop:
    cmp r12, NB_TRIANGLES
    jge .fin
    
    ; Charger le triangle courant
    mov rax, r12
    imul rax, 6
    imul rax, 4
    lea rbx, [triangles + rax]
    
    mov eax, [rbx]
    mov dword[ax], eax
    mov eax, [rbx+4]
    mov dword[ay], eax
    mov eax, [rbx+8]
    mov dword[bx], eax
    mov eax, [rbx+12]
    mov dword[by], eax
    mov eax, [rbx+16]
    mov dword[cx], eax
    mov eax, [rbx+20]
    mov dword[cy], eax
    
    ; Définir la couleur
    mov rax, r12
    imul rax, 4
    lea rbx, [couleurs + rax]
    mov rdi, qword[display_name]
    mov rsi, qword[gc]
    mov edx, [rbx]
    call XSetForeground
    
    ; Remplir
    push r12
    call remplir_triangle
    pop r12
    
    inc r12
    jmp .loop
    
.fin:
    pop r12
    pop rbx
    pop rbp
    ret
generer_couleur_aleatoire:
    ; Génère une couleur RGB aléatoire
    ; Sortie: eax = 0x00RRGGBB
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
