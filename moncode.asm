; ===============================================
; Projet Triangle - Architecture 16 bits
; NASM x86 16-bits
; ===============================================

; Note: En 16 bits, on n'a pas accès à X11
; On utilisera le mode graphique VGA 13h (320x200, 256 couleurs)

BITS 16
ORG 0x100           ; Programme COM DOS

section .text
start:
    ; Initialiser le mode graphique VGA 13h
    mov ax, 0x0013
    int 0x10
    
    ; Générer et dessiner plusieurs triangles
    mov word[nb_triangles_a_dessiner], 5
    call generer_n_triangles
    call dessiner_n_triangles
    
    ; Attendre une touche
    xor ah, ah
    int 0x16
    
    ; Revenir en mode texte
    mov ax, 0x0003
    int 0x10
    
    ; Terminer le programme
    mov ax, 0x4C00
    int 0x21

section .data
    ; Constantes pour les limites de l'écran
    MAX_X equ 320
    MAX_Y equ 200
    
    ; Coordonnées du triangle courant
    ax_coord: dw 0
    ay_coord: dw 0
    bx_coord: dw 0
    by_coord: dw 0
    cx_coord: dw 0
    cy_coord: dw 0
    
    ; Pour stocker plusieurs triangles
    MAX_TRIANGLES equ 10
    triangles: times (MAX_TRIANGLES * 6) dw 0  ; 6 coordonnées par triangle
    couleurs: times MAX_TRIANGLES db 0
    nb_triangles_a_dessiner: dw 0
    
    ; Variable pour générateur aléatoire
    seed: dw 0x1234

section .text

; ===============================================
; Fonction: generer_nombre_aleatoire
; Générateur LCG (Linear Congruential Generator)
; Entrée: AX = valeur maximale
; Sortie: AX = nombre aléatoire entre 0 et max-1
; ===============================================
generer_nombre_aleatoire:
    push bx
    push cx
    push dx
    
    mov bx, ax          ; Sauvegarder max
    
    ; LCG: seed = (seed * 25173 + 13849) & 0xFFFF
    mov ax, word[seed]
    mov cx, 25173
    mul cx              ; DX:AX = AX * CX
    add ax, 13849
    adc dx, 0
    mov word[seed], ax
    
    ; Réduire à la plage [0, max[
    xor dx, dx
    div bx              ; AX = DX:AX / BX, reste dans DX
    mov ax, dx
    
    pop dx
    pop cx
    pop bx
    ret

; ===============================================
; Fonction: generer_triangle_aleatoire
; Génère les coordonnées de 3 points aléatoires
; ===============================================
generer_triangle_aleatoire:
    push ax
    push bx
    
    ; Générer Ax
    mov ax, MAX_X
    call generer_nombre_aleatoire
    mov word[ax_coord], ax
    
    ; Générer Ay
    mov ax, MAX_Y
    call generer_nombre_aleatoire
    mov word[ay_coord], ax
    
    ; Générer Bx
    mov ax, MAX_X
    call generer_nombre_aleatoire
    mov word[bx_coord], ax
    
    ; Générer By
    mov ax, MAX_Y
    call generer_nombre_aleatoire
    mov word[by_coord], ax
    
    ; Générer Cx
    mov ax, MAX_X
    call generer_nombre_aleatoire
    mov word[cx_coord], ax
    
    ; Générer Cy
    mov ax, MAX_Y
    call generer_nombre_aleatoire
    mov word[cy_coord], ax
    
    pop bx
    pop ax
    ret

; ===============================================
; Fonction: put_pixel
; Dessine un pixel à l'écran
; Entrée: AX = x, BX = y, CL = couleur
; ===============================================
put_pixel:
    push ax
    push bx
    push cx
    push dx
    push di
    push es
    
    ; Vérifier les limites
    cmp ax, MAX_X
    jae .fin
    cmp bx, MAX_Y
    jae .fin
    
    ; Calculer l'adresse: offset = y * 320 + x
    push ax             ; Sauvegarder x
    mov ax, bx          ; ax = y
    mov dx, 320
    mul dx              ; dx:ax = y * 320
    pop bx              ; bx = x
    add ax, bx          ; ax = y * 320 + x
    adc dx, 0
    
    ; Adresse segment VGA
    mov di, ax
    mov ax, 0xA000
    mov es, ax
    
    ; Écrire le pixel
    mov byte[es:di], cl
    
.fin:
    pop es
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ===============================================
; Fonction: calculer_determinant
; Calcul du déterminant de deux vecteurs
; Entrée sur la pile (dans l'ordre):
;   [BP+4]  = x1 (premier point du vecteur)
;   [BP+6]  = y1
;   [BP+8]  = x2 (deuxième point vecteur 1)
;   [BP+10] = y2
;   [BP+12] = x3 (deuxième point vecteur 2)
;   [BP+14] = y3
; Sortie: DX:AX = déterminant (signé 32 bits)
; Formule: (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
; ===============================================
calculer_determinant:
    push bp
    mov bp, sp
    push bx
    push cx
    push si
    push di
    
    ; Calculer vecteur 1: (x2-x1, y2-y1)
    mov ax, word[bp+8]  ; x2
    sub ax, word[bp+4]  ; x2 - x1
    mov si, ax          ; si = x du vecteur 1
    
    mov bx, word[bp+10] ; y2
    sub bx, word[bp+6]  ; y2 - y1
    mov di, bx          ; di = y du vecteur 1
    
    ; Calculer vecteur 2: (x3-x1, y3-y1)
    mov cx, word[bp+12] ; x3
    sub cx, word[bp+4]  ; x3 - x1 (x du vecteur 2)
    
    mov bx, word[bp+14] ; y3
    sub bx, word[bp+6]  ; y3 - y1 (y du vecteur 2)
    
    ; Premier produit: si * bx = (x2-x1) * (y3-y1)
    mov ax, si
    imul bx             ; dx:ax = si * bx
    push dx
    push ax             ; Sauvegarder le premier produit
    
    ; Second produit: cx * di = (x3-x1) * (y2-y1)
    mov ax, cx
    imul di             ; dx:ax = cx * di
    
    ; Soustraire: premier - second
    pop bx              ; Partie basse du premier produit
    pop cx              ; Partie haute du premier produit
    
    sub bx, ax          ; Partie basse
    sbb cx, dx          ; Partie haute avec retenue
    
    mov ax, bx
    mov dx, cx
    
    pop di
    pop si
    pop cx
    pop bx
    pop bp
    ret

; ===============================================
; Fonction: est_triangle_direct
; Détermine si le triangle ABC est direct
; Sortie: AX = 1 si direct, 0 si indirect
; ===============================================
est_triangle_direct:
    push bp
    mov bp, sp
    sub sp, 12          ; Espace pour les 6 paramètres
    
    ; Préparer les paramètres pour calculer_determinant
    ; On calcule le déterminant de BA et BC avec B comme origine
    mov ax, word[bx_coord]
    mov word[bp-12], ax     ; x1 = bx
    mov ax, word[by_coord]
    mov word[bp-10], ax     ; y1 = by
    mov ax, word[ax_coord]
    mov word[bp-8], ax      ; x2 = ax
    mov ax, word[ay_coord]
    mov word[bp-6], ax      ; y2 = ay
    mov ax, word[cx_coord]
    mov word[bp-4], ax      ; x3 = cx
    mov ax, word[cy_coord]
    mov word[bp-2], ax      ; y3 = cy
    
    ; Pousser les paramètres sur la pile
    push word[bp-2]
    push word[bp-4]
    push word[bp-6]
    push word[bp-8]
    push word[bp-10]
    push word[bp-12]
    
    call calculer_determinant
    add sp, 12
    
    ; Vérifier le signe du déterminant (partie haute DX)
    xor bx, bx
    test dx, dx
    jns .positif        ; Si positif ou zéro
    
    ; Négatif = direct
    mov ax, 1
    jmp .fin
    
.positif:
    ; Positif = indirect
    xor ax, ax
    
.fin:
    mov sp, bp
    pop bp
    ret

; ===============================================
; Fonction: point_dans_triangle
; Vérifie si un point P est dans le triangle ABC
; Entrée: AX = px, BX = py
; Sortie: AX = 1 si dedans, 0 sinon
; ===============================================
point_dans_triangle:
    push bp
    mov bp, sp
    sub sp, 20          ; Variables locales
    
    ; [bp-2] = px
    ; [bp-4] = py
    ; [bp-6] = type triangle (1=direct, 0=indirect)
    ; [bp-10] = det1 (32 bits)
    ; [bp-14] = det2 (32 bits)
    ; [bp-18] = det3 (32 bits)
    
    mov word[bp-2], ax  ; Sauvegarder px
    mov word[bp-4], bx  ; Sauvegarder py
    
    ; Déterminer le type de triangle
    call est_triangle_direct
    mov word[bp-6], ax
    
    ; ===== Calcul det1: position par rapport à AB =====
    push word[bp-4]         ; py
    push word[bp-2]         ; px
    push word[by_coord]     ; by
    push word[bx_coord]     ; bx
    push word[ay_coord]     ; ay
    push word[ax_coord]     ; ax
    call calculer_determinant
    add sp, 12
    mov word[bp-10], ax     ; Partie basse det1
    mov word[bp-8], dx      ; Partie haute det1
    
    ; ===== Calcul det2: position par rapport à BC =====
    push word[bp-4]         ; py
    push word[bp-2]         ; px
    push word[cy_coord]     ; cy
    push word[cx_coord]     ; cx
    push word[by_coord]     ; by
    push word[bx_coord]     ; bx
    call calculer_determinant
    add sp, 12
    mov word[bp-14], ax     ; Partie basse det2
    mov word[bp-12], dx     ; Partie haute det2
    
    ; ===== Calcul det3: position par rapport à CA =====
    push word[bp-4]         ; py
    push word[bp-2]         ; px
    push word[ay_coord]     ; ay
    push word[ax_coord]     ; ax
    push word[cy_coord]     ; cy
    push word[cx_coord]     ; cx
    call calculer_determinant
    add sp, 12
    mov word[bp-18], ax     ; Partie basse det3
    mov word[bp-16], dx     ; Partie haute det3
    
    ; Vérifier selon le type de triangle
    mov ax, word[bp-6]
    test ax, ax
    jz .triangle_indirect
    
.triangle_direct:
    ; Triangle direct : tous les déterminants >= 0 (à droite)
    mov dx, word[bp-8]      ; Partie haute det1
    test dx, dx
    js .dehors              ; det1 < 0
    
    mov dx, word[bp-12]     ; Partie haute det2
    test dx, dx
    js .dehors              ; det2 < 0
    
    mov dx, word[bp-16]     ; Partie haute det3
    test dx, dx
    js .dehors              ; det3 < 0
    
    jmp .dedans
    
.triangle_indirect:
    ; Triangle indirect : tous les déterminants <= 0 (à gauche)
    mov dx, word[bp-8]      ; Partie haute det1
    test dx, dx
    jns .dehors             ; det1 >= 0
    
    mov dx, word[bp-12]     ; Partie haute det2
    test dx, dx
    jns .dehors             ; det2 >= 0
    
    mov dx, word[bp-16]     ; Partie haute det3
    test dx, dx
    jns .dehors             ; det3 >= 0
    
.dedans:
    mov ax, 1
    jmp .fin
    
.dehors:
    xor ax, ax
    
.fin:
    mov sp, bp
    pop bp
    ret

; ===============================================
; Fonction: min_de_trois
; Trouve le minimum de trois valeurs
; Entrée: AX, BX, CX = trois valeurs
; Sortie: AX = minimum
; ===============================================
min_de_trois:
    cmp ax, bx
    jle .comp_cx
    mov ax, bx
.comp_cx:
    cmp ax, cx
    jle .fin
    mov ax, cx
.fin:
    ret

; ===============================================
; Fonction: max_de_trois
; Trouve le maximum de trois valeurs
; Entrée: AX, BX, CX = trois valeurs
; Sortie: AX = maximum
; ===============================================
max_de_trois:
    cmp ax, bx
    jge .comp_cx
    mov ax, bx
.comp_cx:
    cmp ax, cx
    jge .fin
    mov ax, cx
.fin:
    ret

; ===============================================
; Fonction: remplir_triangle
; Remplit le triangle avec la couleur donnée
; Entrée: CL = couleur
; ===============================================
remplir_triangle:
    push bp
    mov bp, sp
    sub sp, 10
    
    ; [bp-2] = min_x
    ; [bp-4] = max_x
    ; [bp-6] = min_y
    ; [bp-8] = max_y
    ; [bp-9] = couleur
    
    mov byte[bp-9], cl  ; Sauvegarder la couleur
    
    ; Calculer min_x
    mov ax, word[ax_coord]
    mov bx, word[bx_coord]
    mov cx, word[cx_coord]
    call min_de_trois
    mov word[bp-2], ax
    
    ; Calculer max_x
    mov ax, word[ax_coord]
    mov bx, word[bx_coord]
    mov cx, word[cx_coord]
    call max_de_trois
    mov word[bp-4], ax
    
    ; Calculer min_y
    mov ax, word[ay_coord]
    mov bx, word[by_coord]
    mov cx, word[cy_coord]
    call min_de_trois
    mov word[bp-6], ax
    
    ; Calculer max_y
    mov ax, word[ay_coord]
    mov bx, word[by_coord]
    mov cx, word[cy_coord]
    call max_de_trois
    mov word[bp-8], ax
    
    ; Boucle sur Y
    mov bx, word[bp-6]  ; y = min_y
    
.loop_y:
    cmp bx, word[bp-8]
    jg .fin_loop_y
    
    ; Boucle sur X
    mov si, word[bp-2]  ; x = min_x
    
.loop_x:
    cmp si, word[bp-4]
    jg .fin_loop_x
    
    ; Vérifier si le point (si, bx) est dans le triangle
    push bx
    push si
    
    mov ax, si
    mov bx, word[bp-4]  ; Récupérer bx (y) depuis la pile
    mov bx, word[ss:bp-10] ; y est à [bp-10] temporaire
    
    ; Solution: sauvegarder y dans di
    pop si              ; récupérer x
    pop di              ; récupérer y dans di au lieu de bx
    
    push di
    push si
    
    mov ax, si          ; x
    mov bx, di          ; y
    call point_dans_triangle
    
    pop si
    pop di
    
    test ax, ax
    jz .pas_dessiner
    
    ; Dessiner le pixel
    mov ax, si          ; x
    mov bx, di          ; y
    mov cl, byte[bp-9]  ; couleur
    call put_pixel
    
.pas_dessiner:
    inc si
    jmp .loop_x
    
.fin_loop_x:
    inc di
    mov bx, di
    jmp .loop_y
    
.fin_loop_y:
    mov sp, bp
    pop bp
    ret

; ===============================================
; Fonction: dessiner_ligne
; Dessine une ligne avec l'algorithme de Bresenham
; Entrée: AX=x1, BX=y1, CX=x2, DX=y2, DI=couleur
; ===============================================
dessiner_ligne:
    push bp
    mov bp, sp
    sub sp, 20
    
    ; [bp-2] = x1
    ; [bp-4] = y1
    ; [bp-6] = x2
    ; [bp-8] = y2
    ; [bp-10] = dx (absolu)
    ; [bp-12] = dy (absolu)
    ; [bp-14] = sx (direction x)
    ; [bp-16] = sy (direction y)
    ; [bp-18] = err
    ; [bp-19] = couleur
    
    mov word[bp-2], ax
    mov word[bp-4], bx
    mov word[bp-6], cx
    mov word[bp-8], dx
    mov byte[bp-19], dil
    
    ; Calculer dx = abs(x2 - x1)
    mov ax, cx
    sub ax, word[bp-2]
    jge .dx_positif
    neg ax
.dx_positif:
    mov word[bp-10], ax
    
    ; Calculer dy = abs(y2 - y1)
    mov ax, word[bp-8]
    sub ax, word[bp-4]
    jge .dy_positif
    neg ax
.dy_positif:
    mov word[bp-12], ax
    
    ; Calculer sx
    mov ax, word[bp-6]
    cmp ax, word[bp-2]
    jge .sx_positif
    mov word[bp-14], -1
    jmp .calc_sy
.sx_positif:
    mov word[bp-14], 1
    
.calc_sy:
    ; Calculer sy
    mov ax, word[bp-8]
    cmp ax, word[bp-4]
    jge .sy_positif
    mov word[bp-16], -1
    jmp .init_err
.sy_positif:
    mov word[bp-16], 1
    
.init_err:
    ; err = dx - dy
    mov ax, word[bp-10]
    sub ax, word[bp-12]
    mov word[bp-18], ax
    
.boucle:
    ; Dessiner le pixel courant
    mov ax, word[bp-2]
    mov bx, word[bp-4]
    mov cl, byte[bp-19]
    call put_pixel
    
    ; Vérifier si on a atteint (x2, y2)
    mov ax, word[bp-2]
    cmp ax, word[bp-6]
    jne .continuer
    mov ax, word[bp-4]
    cmp ax, word[bp-8]
    je .fin
    
.continuer:
    ; e2 = 2 * err
    mov ax, word[bp-18]
    shl ax, 1           ; e2 = err * 2
    mov si, ax          ; si = e2
    
    ; Si e2 > -dy
    mov bx, word[bp-12]
    neg bx              ; bx = -dy
    cmp si, bx
    jle .check_dx
    
    ; err -= dy
    mov ax, word[bp-18]
    sub ax, word[bp-12]
    mov word[bp-18], ax
    
    ; x1 += sx
    mov ax, word[bp-14]
    add word[bp-2], ax
    
.check_dx:
    ; Si e2 < dx
    cmp si, word[bp-10]
    jge .boucle
    
    ; err += dx
    mov ax, word[bp-18]
    add ax, word[bp-10]
    mov word[bp-18], ax
    
    ; y1 += sy
    mov ax, word[bp-16]
    add word[bp-4], ax
    
    jmp .boucle
    
.fin:
    mov sp, bp
    pop bp
    ret

; ===============================================
; Fonction: dessiner_contour_triangle
; Dessine le contour du triangle
; Entrée: CL = couleur
; ===============================================
dessiner_contour_triangle:
    push bp
    mov bp, sp
    push di
    
    movzx di, cl        ; Sauvegarder la couleur dans DI
    
    ; Ligne AB
    mov ax, word[ax_coord]
    mov bx, word[ay_coord]
    mov cx, word[bx_coord]
    mov dx, word[by_coord]
    call dessiner_ligne
    
    ; Ligne BC
    mov ax, word[bx_coord]
    mov bx, word[by_coord]
    mov cx, word[cx_coord]
    mov dx, word[cy_coord]
    call dessiner_ligne
    
    ; Ligne CA
    mov ax, word[cx_coord]
    mov bx, word[cy_coord]
    mov cx, word[ax_coord]
    mov dx, word[ay_coord]
    call dessiner_ligne
    
    pop di
    pop bp
    ret

; ===============================================
; Fonction: generer_couleur_aleatoire
; Génère une couleur aléatoire (palette VGA)
; Sortie: AL = couleur (1-255, évite 0 qui est noir)
; ===============================================
generer_couleur_aleatoire:
    push bx
    
    mov ax, 254         ; Max 254
    call generer_nombre_aleatoire
    inc ax              ; Résultat entre 1 et 255
    
    pop bx
    ret

; ===============================================
; Fonction: generer_n_triangles
; Génère N triangles aléatoires
; ===============================================
generer_n_triangles:
    push bp
    mov bp, sp
    push bx
    push si
    push di
    
    xor si, si          ; compteur = 0
    mov di, triangles   ; Pointeur vers le tableau
    
.loop:
    cmp si, word[nb_triangles_a_dessiner]
    jge .fin
    
    ; Générer 6 coordonnées
    mov ax, MAX_X
    call generer_nombre_aleatoire
    mov word[di], ax    ; ax
    add di, 2
    
    mov ax, MAX_Y
    call generer_nombre_aleatoire
    mov word[di], ax    ; ay
    add di, 2
    
    mov ax, MAX_X
    call generer_nombre_aleatoire
    mov word[di], ax    ; bx
    add di, 2
    
    mov ax, MAX_Y
    call generer_nombre_aleatoire
    mov word[di], ax    ; by
    add di, 2
    
    mov ax, MAX_X
    call generer_nombre_aleatoire
    mov word[di], ax    ; cx
    add di, 2
    
    mov ax, MAX_Y
    call generer_nombre_aleatoire
    mov word[di], ax    ; cy
    add di, 2
    
    ; Générer une couleur
    call generer_couleur_aleatoire
    mov bx, si
    mov byte[couleurs + bx], al
    
    inc si
    jmp .loop
    
.fin:
    pop di
    pop si
    pop bx
    pop bp
    ret

; ===============================================
; Fonction: dessiner_n_triangles
; Dessine tous les triangles générés
; ===============================================
dessiner_n_triangles:
    push bp
    mov bp, sp
    push bx
    push si
    push di
    
    xor si, si          ; compteur = 0
    mov di, triangles   ; Pointeur vers le tableau
    
.loop:
    cmp si, word[nb_triangles_a_dessiner]
    jge .fin
    
    ; Charger le triangle courant dans les variables globales
    mov ax, word[di]
    mov word[ax_coord], ax
    mov ax, word[di+2]
    mov word[ay_coord], ax
    mov ax, word[di+4]
    mov word[bx_coord], ax
    mov ax, word[di+6]
    mov word[by_coord], ax
    mov ax, word[di+8]
    mov word[cx_coord], ax
    mov ax, word[di+10]
    mov word[cy_coord], ax
    
    ; Charger la couleur
    mov bx, si
    mov cl, byte[couleurs + bx]
    
    ; Remplir le triangle
    push si
    push di
    call remplir_triangle
    pop di
    pop si
    
    ; Dessiner le contour en noir (couleur 0)
    push si
    push di
    mov cl, 0
    call dessiner_contour_triangle
    pop di
    pop si
    
    ; Passer au triangle suivant
    add di, 12          ; 6 mots de 2 octets
    inc si
    jmp .loop
    
.fin:
    pop di
    pop si
    pop bx
    pop bp
    ret
