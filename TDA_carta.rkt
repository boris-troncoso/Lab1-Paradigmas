#lang racket
(require "TDA_ataque.rkt")
(provide card get-card-type get-card-name get-pokemon-hp get-pokemon-type)

; Descripcion: Funcion constructora multiproposito para crear cartas (Pokemon, Energia o Entrenador).
; Dom: n-variables, depende del tipo de carta
; Rec: card (list)
; Tipo recursion: No aplica
(define (card tipoCarta nombre . args)
  (cons 'card (cons tipoCarta (cons nombre args))))

; Selectores basicos
(define (get-card-type c) (cadr c))
(define (get-card-name c) (caddr c))
(define (get-pokemon-hp c) (car (cddddr c))) 
(define (get-pokemon-type c) (cadr (cddddr c)))