#lang racket
;; ===========================
;; TDA Sistema
;; ===========================

;; Representación: (list fecha usuarios libros prestamos config)
;; - fecha: número o string con la fecha actual
;; - usuarios: lista de usuarios
;; - libros: lista de libros
;; - prestamos: lista de prestamos
;; - config: lista con parámetros (tasa, maxDeuda, maxLibros, etc.)

(define (crear-sistema fecha config)
  (list fecha '() '() '() config))

;; Selectores
(define (sistema-fecha sistema) (list-ref sistema 0))
(define (sistema-usuarios sistema) (list-ref sistema 1))
(define (sistema-libros sistema) (list-ref sistema 2))
(define (sistema-prestamos sistema) (list-ref sistema 3))
(define (sistema-config sistema) (list-ref sistema 4))

;; Modificadores
(define (sistema-actualizar-fecha sistema nuevaFecha)
  (list nuevaFecha
        (sistema-usuarios sistema)
        (sistema-libros sistema)
        (sistema-prestamos sistema)
        (sistema-config sistema)))

(define (sistema-agregar-usuario sistema usuario)
  (list (sistema-fecha sistema)
        (cons usuario (sistema-usuarios sistema))
        (sistema-libros sistema)
        (sistema-prestamos sistema)
        (sistema-config sistema)))

(define (sistema-agregar-libro sistema libro)
  (list (sistema-fecha sistema)
        (sistema-usuarios sistema)
        (cons libro (sistema-libros sistema))
        (sistema-prestamos sistema)
        (sistema-config sistema)))

(define (sistema-agregar-prestamo sistema prestamo)
  (list (sistema-fecha sistema)
        (sistema-usuarios sistema)
        (sistema-libros sistema)
        (cons prestamo (sistema-prestamos sistema))
        (sistema-config sistema)))
