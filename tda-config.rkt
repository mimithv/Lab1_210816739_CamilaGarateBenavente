#lang racket
;; tda-config.rkt
;; -----------------------------------------------------------------------------
;; Parámetros globales del sistema.
;; max-libros-usuario, dias-max-prestamo, tasa-multa-diaria, limite-deuda-max,
;; dias-max-retraso, fecha-inicial (string "DD/MM").
;; -----------------------------------------------------------------------------
(require "util.rkt" "tda-fecha.rkt")
(provide make-config config? cfg-max-libros cfg-dias-max cfg-tasa-multa
         cfg-limite-deuda cfg-max-retraso cfg-fecha-inicial)

;; Representación: (list 'config max-libros dias-max tasa limite max-retraso fecha-inicial)
(define (make-config max-libros dias-max tasa limite max-retraso fecha-inicial)
  (list 'config
        (ensure-nat+ max-libros 'max-libros-usuario)
        (ensure-nat+ dias-max 'dias-max-prestamo)
        (ensure-nat  tasa 'tasa-multa-diaria)
        (ensure-nat  limite 'limite-deuda-max)
        (ensure-nat  max-retraso 'dias-max-retraso)
        (make-fecha  fecha-inicial)))

(define (config? c) (and (pair? c) (eq? (car c) 'config)))
(define (cfg-max-libros c)    (cadr c))
(define (cfg-dias-max c)      (caddr c))
(define (cfg-tasa-multa c)    (cadddr c))
(define (cfg-limite-deuda c)  (list-ref c 4))
(define (cfg-max-retraso c)   (list-ref c 5))
(define (cfg-fecha-inicial c) (list-ref c 6))
