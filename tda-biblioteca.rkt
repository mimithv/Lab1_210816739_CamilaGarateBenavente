#lang racket
;; tda-biblioteca.rkt
;; -----------------------------------------------------------------------------
;; Biblioteca: estado global del sistema para RF posteriores.
;; Incluye: fecha-actual, config, listas de libros, usuarios y préstamos.
;; -----------------------------------------------------------------------------
(require "util.rkt" "tda-config.rkt" "tda-libro.rkt" "tda-usuario.rkt" "tda-prestamo.rkt" "tda-fecha.rkt")
(provide make-biblioteca biblioteca? bib-fecha bib-config bib-libros bib-usuarios bib-prestamos
         bib-con-fecha bib-con-libros bib-con-usuarios bib-con-prestamos)

;; Representación: (list 'biblioteca fecha-actual config libros usuarios prestamos)
;; fecha-actual : string "DD/MM"
;; config       : TDA Config
;; libros       : (listof Libro)
;; usuarios     : (listof Usuario)
;; prestamos    : (listof Prestamo)
(define (make-biblioteca fecha-actual config libros usuarios prestamos)
  (when (not (config? config)) (error 'make-biblioteca "config inválida"))
  (when (not (and (list? libros)   (andmap libro? libros)))     (error 'make-biblioteca "libros inválidos"))
  (when (not (and (list? usuarios) (andmap usuario? usuarios))) (error 'make-biblioteca "usuarios inválidos"))
  (when (not (and (list? prestamos) (andmap prestamo? prestamos))) (error 'make-biblioteca "prestamos inválidos"))
  (list 'biblioteca (make-fecha fecha-actual) config libros usuarios prestamos))

(define (biblioteca? b) (and (pair? b) (eq? (car b) 'biblioteca)))
(define (bib-fecha b)     (cadr b))
(define (bib-config b)    (caddr b))
(define (bib-libros b)    (cadddr b))
(define (bib-usuarios b)  (list-ref b 4))
(define (bib-prestamos b) (list-ref b 5))

;; Modificadores puros
(define (bib-con-fecha b f2)
  (list 'biblioteca (make-fecha f2) (bib-config b) (bib-libros b) (bib-usuarios b) (bib-prestamos b)))

(define (bib-con-libros b nuevos)
  (when (not (and (list? nuevos) (andmap libro? nuevos))) (error 'bib-con-libros "lista de libros inválida"))
  (list 'biblioteca (bib-fecha b) (bib-config b) nuevos (bib-usuarios b) (bib-prestamos b)))

(define (bib-con-usuarios b nuevos)
  (when (not (and (list? nuevos) (andmap usuario? nuevos))) (error 'bib-con-usuarios "lista de usuarios inválida"))
  (list 'biblioteca (bib-fecha b) (bib-config b) (bib-libros b) nuevos (bib-prestamos b)))

(define (bib-con-prestamos b nuevos)
  (when (not (and (list? nuevos) (andmap prestamo? nuevos))) (error 'bib-con-prestamos "lista de préstamos inválida"))
  (list 'biblioteca (bib-fecha b) (bib-config b) (bib-libros b) (bib-usuarios b) nuevos))
