#lang racket
(provide deck shuffleDeck randomPuro)

; Descripcion: Funcion que permite crear un mazo de cartas.
; Dom: cartas (List card)
; Rec: deck (list)
; Tipo recursion: No aplica
(define (deck . cartas)
  cartas)

; Descripcion: Generador de numeros pseudoaleatorios entregado en el enunciado.
; Dom: Xn (int)
; Rec: int
; Tipo recursion: No aplica
(define (randomPuro Xn)
  (modulo (+ (* Xn 1103515245) 12345) 2147483648))

; Descripcion: Revuelve una baraja de juegos haciendo uso de una semilla.
; Dom: mazo (deck) X seed (int+)
; Rec: deck (list)
; Tipo recursion: Natural (a traves de map y sort)
(define (shuffleDeck mazo seed)
  (define (tag-cards m s)
    (if (null? m) 
        '()
        (let ((next-seed (randomPuro s)))
          (cons (cons next-seed (car m))
                (tag-cards (cdr m) next-seed)))))
  ; Ordena las cartas con el número pseudoaleatorio generado
  (map cdr (sort (tag-cards mazo seed) (lambda (a b) (< (car a) (car b))))))