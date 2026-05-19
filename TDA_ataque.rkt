#lang racket
(provide attack get-attack-cost get-attack-name get-attack-text get-attack-fn)

; Descripcion: Funcion constructora para TDA Ataque. Crea un ataque con su costo, nombre, texto y funcion.
; Dom: cost (List ELEMENT-TYPE) X nombre (string) X texto (string) X funcion (procedure)
; Rec: attack (list)
; Tipo recursión: No aplica
(define (attack cost nombre texto funcion)
  (list 'attack cost nombre texto funcion))

; Selectores básicos
(define (get-attack-cost a) (cadr a))
(define (get-attack-name a) (caddr a))
(define (get-attack-text a) (cadddr a))
(define (get-attack-fn a) (car (cddddr a)))