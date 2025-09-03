#lang racket
;; tda-libro.rkt
;; -----------------------------------------------------------------------------
;; Libro: id único, título, autor, disponibilidad.
;; Reglas: título/autor en minúsculas al crear.
;; -----------------------------------------------------------------------------
(require "util.rkt")
(provide make-libro libro? libro-id libro-titulo libro-autor libro-disponible?
         libro-marcar-disponible libro-marcar-no-disponible)

;; Representación: (list 'libro id titulo autor disponible?)
(define (make-libro id titulo autor)
  (list 'libro (ensure-nat+ id 'id) (str-low titulo) (str-low autor) #t))

(define (libro? x) (and (pair? x) (eq? (car x) 'libro)))
(define (libro-id L)         (cadr L))
(define (libro-titulo L)     (caddr L))
(define (libro-autor L)      (cadddr L))
(define (libro-disponible? L) (list-ref L 4))

;; Modificadores puros
(define (libro-marcar-disponible L)
  (list 'libro (libro-id L) (libro-titulo L) (libro-autor L) #t))

(define (libro-marcar-no-disponible L)
  (list 'libro (libro-id L) (libro-titulo L) (libro-autor L) #f))
