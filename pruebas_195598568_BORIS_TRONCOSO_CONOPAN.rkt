#lang racket
;; Importamos el archivo principal (asegúrate de que el nombre coincida exactamente)
(require "main_195598568_BORIS_TRONCOSO_CONOPAN.rkt")

;; ---------------------------------------------------------
;; RNF10: Script de Pruebas
;; ---------------------------------------------------------

;; Funciones auxiliares para los efectos de los ataques
(define f-sin-efecto (lambda (juego args) juego))
(define f-dormir (lambda (juego args) juego))
(define f-paralizar (lambda (juego args) juego))

;; Creación de algunos ataques base para probar
(define a1 (attack '(psychic) "Pequeña Rabieta" "Inflige 20 de daño" f-sin-efecto))
(define a2 (attack '(psychic colorless) "Pesadilla" "Lanza 1 moneda. Si sale cara, el rival queda Dormido" f-dormir))

(display "--- Archivo de pruebas iniciado y enlazado correctamente ---\n")