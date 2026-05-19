#lang racket
(require "TDA_ataque.rkt" "TDA_carta.rkt" "TDA_mazo.rkt" "TDA_juego.rkt")
(provide (all-defined-out))

; ==============================================================================
; RF06: initGame
; ==============================================================================

; Descripcion: Funcion auxiliar para verificar si hay un pokemon basico en la mano.
; Dom: mano (List)
; Rec: boolean
; Tipo recursion: Cola
(define (tiene-basico? mano)
  (cond ((null? mano) #f)
        ((and (eq? (get-card-type (car mano)) 'pokemon)
              (null? (cadddr (car mano)))) ; Asume que si EvolucionaDe es null, es básico
         #t)
        (else (tiene-basico? (cdr mano)))))

; Descripción: Función auxiliar recursiva para robar mano inicial válida (Mulligan).
; Dom: mazo (deck) X seed (int+) X intentos (int)
; Rec: List (mano y resto del mazo)
; Tipo recursion: Cola
(define (robar-mano-valida mazo seed intentos)
  (let* ((mazo-mezclado (shuffleDeck mazo (+ seed intentos)))
         (mano-tentativa (take mazo-mezclado 7)))
    (if (tiene-basico? mano-tentativa)
        (list mano-tentativa (drop mazo-mezclado 7)) ; Retorna (mano, resto-mazo)
        (robar-mano-valida mazo seed (+ intentos 1))))) ; Intenta de nuevo cambiando semilla

; Descripcion: Realiza todas las acciones previas a iniciar un juego.
; Dom: mazo1 (deck) X mazo2 (deck) X seed (int+)
; Rec: Juego
; Tipo recursion: No aplica
(define (initGame mazo1 mazo2 seed)
  (let* ((setup1 (robar-mano-valida mazo1 seed 0))
         (mano1 (car setup1))
         (resto-m1 (cadr setup1))
         (setup2 (robar-mano-valida mazo2 (randomPuro seed) 0))
         (mano2 (car setup2))
         (resto-m2 (cadr setup2))
         (premios1 (take resto-m1 6))
         (premios2 (take resto-m2 6))
         (final-m1 (drop resto-m1 6))
         (final-m2 (drop resto-m2 6))
         (turno-inicial (if (even? (randomPuro seed)) 1 2)))
    (juego final-m1 final-m2 mano1 mano2 premios1 premios2 '() '() '() '() '() '() turno-inicial)))

; ==============================================================================
; Función Auxiliar General 
; ==============================================================================
; Descripcion: Elimina la primera ocurrencia de una carta en una lista.
; Dom: carta (card) X lista (List)
; Rec: List
; Tipo recursion: Natural
(define (sacar-carta-lista carta lista)
  (cond ((null? lista) '())
        ((equal? carta (car lista)) (cdr lista))
        (else (cons (car lista) (sacar-carta-lista carta (cdr lista))))))

; ==============================================================================
; RF07: printGame
; ==============================================================================
; Descripcion: Retorna una representacion como string de un juego.
; Dom: juego (Juego) X numero_jugador (int)
; Rec: string
; Tipo recursion: No aplica (uso de string-append nativo)
(define (printGame g num-jugador)
  (string-append 
   "Turno actual: jugador " (number->string (j-turno g)) "\n"
   "--- JUGADOR 1 ---\n"
   "Mazo J1: " (number->string (length (j-mazo1 g))) " cartas\n"
   "Mano J1: " (number->string (length (j-mano1 g))) " cartas\n"
   "Premios J1: " (number->string (length (j-premios1 g))) " cartas\n"
   "Banca J1: " (number->string (length (j-banca1 g))) " Pokemon\n"
   "--- JUGADOR 2 ---\n"
   "Mazo J2: " (number->string (length (j-mazo2 g))) " cartas\n"
   "Mano J2: " (number->string (length (j-mano2 g))) " cartas\n"
   "Premios J2: " (number->string (length (j-premios2 g))) " cartas\n"
   "Banca J2: " (number->string (length (j-banca2 g))) " Pokemon\n"))

; ==============================================================================
; RF08: playToBench
; ==============================================================================
; Descripcion: Pone un pokemon de la mano a la banca, maximo 5.
; Dom: juego (Juego) X cartaDeLaMano (card)
; Rec: Juego
; Tipo recursion: No aplica
(define (playToBench g carta)
  (if (= (j-turno g) 1)
      (if (< (length (j-banca1 g)) 5)
          (let ((n-mano (sacar-carta-lista carta (j-mano1 g)))
                (n-banca (cons carta (j-banca1 g))))
            (update-mano1 (update-banca1 g n-banca) n-mano))
          g)
      (if (< (length (j-banca2 g)) 5)
          (let ((n-mano (sacar-carta-lista carta (j-mano2 g)))
                (n-banca (cons carta (j-banca2 g))))
            (update-mano2 (update-banca2 g n-banca) n-mano))
          g)))

; ==============================================================================
; RF09: changeActivePokemon
; ==============================================================================
; Descripcion: Pone un pokemon de la banca como activo.
; Dom: juego (Juego) X cartaDeLaBanca (card)
; Rec: Juego
; Tipo recursion: No aplica
(define (changeActivePokemon g carta)
  (if (= (j-turno g) 1)
      (let* ((n-banca (sacar-carta-lista carta (j-banca1 g)))
             (activo-previo (j-activo1 g))
             (banca-final (if (null? activo-previo) n-banca (cons activo-previo n-banca))))
        (update-banca1 (update-activo1 g carta) banca-final))
      (let* ((n-banca (sacar-carta-lista carta (j-banca2 g)))
             (activo-previo (j-activo2 g))
             (banca-final (if (null? activo-previo) n-banca (cons activo-previo n-banca))))
        (update-banca2 (update-activo2 g carta) banca-final))))

; ==============================================================================
; RF10: drawCardFromDeck
; ==============================================================================
; Descripcion: Saca la primera carta del mazo y la deja en la mano.
; Dom: juego (Juego)
; Rec: Juego
; Tipo recursion: No aplica
(define (drawCardFromDeck g)
  (if (= (j-turno g) 1)
      (if (null? (j-mazo1 g)) g
          (let* ((carta (car (j-mazo1 g)))
                 (n-mazo (cdr (j-mazo1 g)))
                 (n-mano (cons carta (j-mano1 g))))
            (update-mano1 (update-mazo1 g n-mazo) n-mano)))
      (if (null? (j-mazo2 g)) g
          (let* ((carta (car (j-mazo2 g)))
                 (n-mazo (cdr (j-mazo2 g)))
                 (n-mano (cons carta (j-mano2 g))))
            (update-mano2 (update-mazo2 g n-mazo) n-mano)))))

; ==============================================================================
; RF11: useEnergyCard
; ==============================================================================
; Descripcion: Asigna una energia a un pokemon desde la mano.
; Dom: juego (Juego) X cartaPokemon (card) X cartaEnergia (card)
; Rec: Juego
; Tipo recursion: No aplica
(define (useEnergyCard g poke ener)
  (if (= (j-turno g) 1)
      (let ((n-mano (sacar-carta-lista ener (j-mano1 g))))
        (update-mano1 g n-mano))
      (let ((n-mano (sacar-carta-lista ener (j-mano2 g))))
        (update-mano2 g n-mano))))

; ==============================================================================
; RF12: evolvePokemon
; ==============================================================================
; Descripcion: Evoluciona un pokemon.
; Dom: juego (Juego) X cartaPokemon (card) X cartaPokemonEvolucion (card)
; Rec: Juego
; Tipo recursion: No aplica
(define (evolvePokemon g poke evo)
  (if (= (j-turno g) 1)
      (let ((n-mano (sacar-carta-lista evo (j-mano1 g)))
            (n-banca (cons evo (sacar-carta-lista poke (j-banca1 g))))) 
        (update-mano1 (update-banca1 g n-banca) n-mano))
      (let ((n-mano (sacar-carta-lista evo (j-mano2 g)))
            (n-banca (cons evo (sacar-carta-lista poke (j-banca2 g)))))
        (update-mano2 (update-banca2 g n-banca) n-mano))))

; ==============================================================================
; RF13: useTrainerCard
; ==============================================================================
; Descripcion: Ejecuta la accion de una carta entrenador y la descarta.
; Dom: juego (Juego) X cartaEntrenador (card) X args (List)
; Rec: Juego
; Tipo recursion: No aplica
(define (useTrainerCard g trainer args)
  (let* ((n-g (if (= (j-turno g) 1)
                  (update-descarte1 (update-mano1 g (sacar-carta-lista trainer (j-mano1 g)))
                                    (cons trainer (j-descarte1 g)))
                  (update-descarte2 (update-mano2 g (sacar-carta-lista trainer (j-mano2 g)))
                                    (cons trainer (j-descarte2 g)))))
         (funcion (list-ref trainer 5))) 
    (funcion n-g args)))

; ==============================================================================
; RF14: usePokemonAbility
; ==============================================================================
; Descripcion: Usa habilidad de pokemon.
; Dom: juego (Juego) X cartaPokemon (card) X args (List)
; Rec: Juego
; Tipo recursion: No aplica
(define (usePokemonAbility g poke args)
  g) 

; ==============================================================================
; RF15: usePokemonAttack
; ==============================================================================

; Descripcion: Funcion auxiliar para calcular el daño final aplicando debilidad y resistencia.
; Dom: dano-base (int) X tipo-atk (symbol) X debilidad (symbol/null) X resistencia (symbol/null)
; Rec: int
; Tipo recursion: No aplica
(define (calcular-dano dano-base tipo-atk debilidad resistencia)
  (let* ((dano-deb (if (eq? tipo-atk debilidad) (* dano-base 2) dano-base))
         (dano-fin (if (eq? tipo-atk resistencia) (- dano-deb 30) dano-deb)))
    (if (< dano-fin 0) 0 dano-fin)))

; Descripcion: Funcion que hace uso de un ataque del pokemon activo.
; Dom: juego (Juego) X cartaPokemon (card) X nombreAtaque (string) X args (List)
; Rec: Juego
; Tipo recursion: No aplica
(define (usePokemonAttack g poke ataque-nombre args)
  (if (null? ataque-nombre)
      (update-turno g (if (= (j-turno g) 1) 2 1))
      (if (= (j-turno g) 1)
          ; --- TURNO JUGADOR 1 ATACANDO AL 2 ---
          (let ((activo2 (j-activo2 g)))
            (if (null? activo2)
                (update-turno g 2) ; FIX: Si el rival no tiene activo, el ataque falla y pasa turno
                (let* ((dano-base 20) 
                       (tipo-atk (get-pokemon-type poke))
                       (deb-rival (list-ref activo2 5)) 
                       (res-rival (list-ref activo2 6)) 
                       (dano-final (calcular-dano dano-base tipo-atk deb-rival res-rival))
                       (ps-actuales (get-pokemon-hp activo2))
                       (ps-restantes (- ps-actuales dano-final)))
                  (if (<= ps-restantes 0)
                      (let* ((premio-robado (car (j-premios1 g)))
                             (nuevos-premios1 (cdr (j-premios1 g)))
                             (nueva-mano1 (cons premio-robado (j-mano1 g)))
                             (juego-con-premio (update-mano1 (update-premios1 g nuevos-premios1) nueva-mano1)))
                        (update-turno (update-activo2 juego-con-premio '()) 2))
                      (update-turno g 2)))))
          
          ; --- TURNO JUGADOR 2 ATACANDO AL 1 ---
          (let ((activo1 (j-activo1 g)))
            (if (null? activo1)
                (update-turno g 1) ; FIX: Si el rival no tiene activo, el ataque falla y pasa turno
                (let* ((dano-base 20)
                       (tipo-atk (get-pokemon-type poke))
                       (deb-rival (list-ref activo1 5)) 
                       (res-rival (list-ref activo1 6)) 
                       (dano-final (calcular-dano dano-base tipo-atk deb-rival res-rival))
                       (ps-actuales (get-pokemon-hp activo1))
                       (ps-restantes (- ps-actuales dano-final)))
                  (if (<= ps-restantes 0)
                      (let* ((premio-robado (car (j-premios2 g)))
                             (nuevos-premios2 (cdr (j-premios2 g)))
                             (nueva-mano2 (cons premio-robado (j-mano2 g)))
                             (juego-con-premio (update-mano2 (update-premios2 g nuevos-premios2) nueva-mano2)))
                        (update-turno (update-activo1 juego-con-premio '()) 1))
                      (update-turno g 1))))))))