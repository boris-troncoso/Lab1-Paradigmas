#lang racket

(require "main_195598568_BORIS_TRONCOSO_CONOPAN.rkt") 
(require "TDA_ataque.rkt")
(require "TDA_carta.rkt")
(require "TDA_mazo.rkt")
(require "TDA_juego.rkt")

;; Funciones dummy para acciones (cumplen firma de orden superior)
(define accion-nula (lambda (juego args) juego))
(define accion-robar (lambda (juego args) juego))
(define accion-curar (lambda (juego args) juego))

;; RF02: attack (3 usos)
(define atq1 (attack '(fire) "Llama Pequeña" "Inflige 10 de daño" accion-nula))
(define atq2 (attack '(water water) "Chorro de Agua" "Inflige 40 de daño" accion-nula))
(define atq3 (attack '(grass colorless) "Latigo Cepa" "Cura 10 PS" accion-curar))

;; RF03: card (3 usos: energía, pokémon, entrenador)
(define c-fuego (card 'energy "Energia Fuego" 'fire))
(define c-poke1 (card 'pokemon "Vulpix" null 50 'fire 'water 'grass 1 #f null (list atq1)))
(define c-poke2 (card 'pokemon "Ninetales" "Vulpix" 90 'fire 'water 'grass 2 #f null (list atq1)))
(define c-train1 (card 'trainer "Pocion" "objeto" "Cura 20 PS" accion-curar))

;; RF04: deck (3 usos) 
(define mazo1 (apply deck (append (make-list 20 c-poke1) (make-list 40 c-fuego))))
(define mazo2 (apply deck (append (make-list 30 c-poke1) (make-list 30 c-fuego))))
(define mazo3 (apply deck (append (make-list 15 c-poke1) (make-list 15 c-poke2) (make-list 10 c-train1) (make-list 20 c-fuego))))

;; RF05: shuffleDeck (3 usos)
(define mazo-mezclado1 (shuffleDeck mazo1 111))
(define mazo-mezclado2 (shuffleDeck mazo2 222))
(define mazo-mezclado3 (shuffleDeck mazo3 333))

;; RF06: initGame (3 usos)
(define j1 (initGame mazo-mezclado2 mazo-mezclado2 123))
(define j2 (initGame mazo-mezclado2 mazo-mezclado3 456))
(define j3 (initGame mazo-mezclado3 mazo-mezclado2 789))

;; RF07: printGame (3 usos)
(displayln (printGame j1 1))
(displayln (printGame j2 2))
(displayln (printGame j3 1))


;; Se encadenan los estados inmutables

;; RF10: drawCardFromDeck (3 usos)
(define s1 (drawCardFromDeck j1))
(define s2 (drawCardFromDeck s1))
(define s3 (drawCardFromDeck s2))

;; RF08: playToBench (3 usos)
(define s4 (playToBench s3 c-poke1))
(define s5 (playToBench s4 c-poke1))
(define s6 (playToBench s5 c-poke1))

;; RF09: changeActivePokemon (3 usos)
(define s7 (changeActivePokemon s6 c-poke1))
(define s8 (changeActivePokemon s7 c-poke1))
(define s9 (changeActivePokemon s8 c-poke1))

;; RF11: useEnergyCard (3 usos)
(define s10 (useEnergyCard s9 c-poke1 c-fuego))
(define s11 (useEnergyCard s10 c-poke1 c-fuego))
(define s12 (useEnergyCard s11 c-poke1 c-fuego))

;; RF12: evolvePokemon (3 usos)
(define s13 (evolvePokemon s12 c-poke1 c-poke2))
(define s14 (evolvePokemon s13 c-poke1 c-poke2))
(define s15 (evolvePokemon s14 c-poke1 c-poke2))

;; RF13: useTrainerCard (3 usos)
(define s16 (useTrainerCard s15 c-train1 '()))
(define s17 (useTrainerCard s16 c-train1 '()))
(define s18 (useTrainerCard s17 c-train1 '()))

;; RF14: usePokemonAbility (3 usos)
(define s19 (usePokemonAbility s18 c-poke2 '()))
(define s20 (usePokemonAbility s19 c-poke2 '()))
(define s21 (usePokemonAbility s20 c-poke2 '()))

;; RF15: usePokemonAttack (3 usos)
(define s22 (usePokemonAttack s21 c-poke2 "Llama Pequeña" '()))
(define s23 (usePokemonAttack s22 c-poke2 null '()))
(define s24 (usePokemonAttack s23 c-poke2 "Llama Pequeña" '()))

(displayln "Todo super!")