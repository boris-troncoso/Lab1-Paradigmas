#lang racket
(provide (all-defined-out))

; ==============================================================================
; TDA JUEGO: Representacion y Constructor
; ==============================================================================

; Descripcion: Constructor del estado de juego para almacenar toda la informacion de la partida.
; Dom: mazo1 X mazo2 X mano1 X mano2 X premios1 X premios2 X banca1 X banca2 X activo1 X activo2 X descarte1 X descarte2 X turnoActivo
; Rec: Juego (list)
; Tipo recursion: No aplica
(define (juego m1 m2 man1 man2 p1 p2 b1 b2 a1 a2 desc1 desc2 turno)
  (list 'juego m1 m2 man1 man2 p1 p2 b1 b2 a1 a2 desc1 desc2 turno))

; ==============================================================================
; TDA JUEGO: Funcion de Pertenencia
; ==============================================================================

; Descripcion: Verifica si un elemento es del tipo TDA Juego.
; Dom: g (any)
; Rec: boolean
; Tipo recursion: No aplica
(define (juego? g)
  (and (list? g) (= (length g) 14) (eq? (car g) 'juego)))

; ==============================================================================
; TDA JUEGO: Selectores
; ==============================================================================

; Descripcion: Obtiene el mazo del jugador 1
; Dom: juego (Juego)
; Rec: List (mazo)
; Tipo recursion: No aplica
(define (j-mazo1 g) (list-ref g 1))

; Descripcion: Obtiene el mazo del jugador 2
; Dom: juego (Juego)
; Rec: List (mazo)
; Tipo recursion: No aplica
(define (j-mazo2 g) (list-ref g 2))

; Descripcion: Obtiene la mano del jugador 1
; Dom: juego (Juego)
; Rec: List (mano)
; Tipo recursion: No aplica
(define (j-mano1 g) (list-ref g 3))

; Descripcion: Obtiene la mano del jugador 2
; Dom: juego (Juego)
; Rec: List (mano)
; Tipo recursion: No aplica
(define (j-mano2 g) (list-ref g 4))

; Descripcion: Obtiene los premios del jugador 1
; Dom: juego (Juego)
; Rec: List (premios)
; Tipo recursion: No aplica
(define (j-premios1 g) (list-ref g 5))

; Descripcion: Obtiene los premios del jugador 2
; Dom: juego (Juego)
; Rec: List (premios)
; Tipo recursion: No aplica
(define (j-premios2 g) (list-ref g 6))

; Descripcion: Obtiene la banca del jugador 1
; Dom: juego (Juego)
; Rec: List (banca)
; Tipo recursion: No aplica
(define (j-banca1 g) (list-ref g 7))

; Descripcion: Obtiene la banca del jugador 2
; Dom: juego (Juego)
; Rec: List (banca)
; Tipo recursion: No aplica
(define (j-banca2 g) (list-ref g 8))

; Descripcion: Obtiene el pokemon activo del jugador 1
; Dom: juego (Juego)
; Rec: card (pokemon) o null
; Tipo recursion: No aplica
(define (j-activo1 g) (list-ref g 9))

; Descripcion: Obtiene el pokemon activo del jugador 2
; Dom: juego (Juego)
; Rec: card (Pokémon) o null
; Tipo recursion: No aplica
(define (j-activo2 g) (list-ref g 10))

; Descripcion: Obtiene la pila de descarte del jugador 1
; Dom: juego (Juego)
; Rec: List (cartas)
; Tipo recursion: No aplica
(define (j-descarte1 g) (list-ref g 11))

; Descripcion: Obtiene la pila de descarte del jugador 2
; Dom: juego (Juego)
; Rec: List (cartas)
; Tipo recursion: No aplica
(define (j-descarte2 g) (list-ref g 12))

; Descripcion: Obtiene el turno actual (1 o 2)
; Dom: juego (Juego)
; Rec: int
; Tipo recursion: No aplica
(define (j-turno g) (list-ref g 13))

; ==============================================================================
; TDA JUEGO: Modificadores retornan un nuevo estado de juego inmutable
; ==============================================================================

(define (update-mazo1 g nuevo-mazo)
  (juego nuevo-mazo (j-mazo2 g) (j-mano1 g) (j-mano2 g) (j-premios1 g) (j-premios2 g) (j-banca1 g) (j-banca2 g) (j-activo1 g) (j-activo2 g) (j-descarte1 g) (j-descarte2 g) (j-turno g)))

(define (update-mazo2 g nuevo-mazo)
  (juego (j-mazo1 g) nuevo-mazo (j-mano1 g) (j-mano2 g) (j-premios1 g) (j-premios2 g) (j-banca1 g) (j-banca2 g) (j-activo1 g) (j-activo2 g) (j-descarte1 g) (j-descarte2 g) (j-turno g)))

(define (update-mano1 g nueva-mano)
  (juego (j-mazo1 g) (j-mazo2 g) nueva-mano (j-mano2 g) (j-premios1 g) (j-premios2 g) (j-banca1 g) (j-banca2 g) (j-activo1 g) (j-activo2 g) (j-descarte1 g) (j-descarte2 g) (j-turno g)))

(define (update-mano2 g nueva-mano)
  (juego (j-mazo1 g) (j-mazo2 g) (j-mano1 g) nueva-mano (j-premios1 g) (j-premios2 g) (j-banca1 g) (j-banca2 g) (j-activo1 g) (j-activo2 g) (j-descarte1 g) (j-descarte2 g) (j-turno g)))

(define (update-premios1 g nuevos-premios)
  (juego (j-mazo1 g) (j-mazo2 g) (j-mano1 g) (j-mano2 g) nuevos-premios (j-premios2 g) (j-banca1 g) (j-banca2 g) (j-activo1 g) (j-activo2 g) (j-descarte1 g) (j-descarte2 g) (j-turno g)))

(define (update-premios2 g nuevos-premios)
  (juego (j-mazo1 g) (j-mazo2 g) (j-mano1 g) (j-mano2 g) (j-premios1 g) nuevos-premios (j-banca1 g) (j-banca2 g) (j-activo1 g) (j-activo2 g) (j-descarte1 g) (j-descarte2 g) (j-turno g)))

(define (update-banca1 g nueva-banca)
  (juego (j-mazo1 g) (j-mazo2 g) (j-mano1 g) (j-mano2 g) (j-premios1 g) (j-premios2 g) nueva-banca (j-banca2 g) (j-activo1 g) (j-activo2 g) (j-descarte1 g) (j-descarte2 g) (j-turno g)))

(define (update-banca2 g nueva-banca)
  (juego (j-mazo1 g) (j-mazo2 g) (j-mano1 g) (j-mano2 g) (j-premios1 g) (j-premios2 g) (j-banca1 g) nueva-banca (j-activo1 g) (j-activo2 g) (j-descarte1 g) (j-descarte2 g) (j-turno g)))

(define (update-activo1 g nuevo-activo)
  (juego (j-mazo1 g) (j-mazo2 g) (j-mano1 g) (j-mano2 g) (j-premios1 g) (j-premios2 g) (j-banca1 g) (j-banca2 g) nuevo-activo (j-activo2 g) (j-descarte1 g) (j-descarte2 g) (j-turno g)))

(define (update-activo2 g nuevo-activo)
  (juego (j-mazo1 g) (j-mazo2 g) (j-mano1 g) (j-mano2 g) (j-premios1 g) (j-premios2 g) (j-banca1 g) (j-banca2 g) (j-activo1 g) nuevo-activo (j-descarte1 g) (j-descarte2 g) (j-turno g)))

(define (update-descarte1 g nuevo-descarte)
  (juego (j-mazo1 g) (j-mazo2 g) (j-mano1 g) (j-mano2 g) (j-premios1 g) (j-premios2 g) (j-banca1 g) (j-banca2 g) (j-activo1 g) (j-activo2 g) nuevo-descarte (j-descarte2 g) (j-turno g)))

(define (update-descarte2 g nuevo-descarte)
  (juego (j-mazo1 g) (j-mazo2 g) (j-mano1 g) (j-mano2 g) (j-premios1 g) (j-premios2 g) (j-banca1 g) (j-banca2 g) (j-activo1 g) (j-activo2 g) (j-descarte1 g) nuevo-descarte (j-turno g)))

(define (update-turno g nuevo-turno)
  (juego (j-mazo1 g) (j-mazo2 g) (j-mano1 g) (j-mano2 g) (j-premios1 g) (j-premios2 g) (j-banca1 g) (j-banca2 g) (j-activo1 g) (j-activo2 g) (j-descarte1 g) (j-descarte2 g) nuevo-turno))