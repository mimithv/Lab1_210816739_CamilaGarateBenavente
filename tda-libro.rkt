#lang racket

(provide crear-libro
         libro-id libro-titulo libro-autor libro-estado
         libro-actualizar-estado get-libro-id)
;; ===========================
;; TDA Libro
;; ===========================

;; Representación: (list ID titulo autor estado)
;; - ID: string o número
;; - titulo: string
;; - autor: string
;; - estado: "disponible" o "no disponible"

;; ==========================================
;; RF03 - crear-libro
;; ==========================================
;; Dom -> Rec: (int x string x string) -> Libro
;; Cabecera: (crear-libro id titulo autor)

(define (crear-libro id titulo autor)
  (list id
        (string-downcase titulo)
        (string-downcase autor)
        "disponible"))

;; ==========================================
;; Selectores
;; ==========================================

(define (libro-id libro) (list-ref libro 0))
(define (libro-titulo libro) (list-ref libro 1))
(define (libro-autor libro) (list-ref libro 2))
(define (libro-estado libro) (list-ref libro 3))

;; ==========================================
;; Modificadores
;; ==========================================

(define (libro-actualizar-estado libro nuevoEstado)
  (list (libro-id libro)
        (libro-titulo libro)
        (libro-autor libro)
        nuevoEstado))

;; ===========================
;; RF10 - get-libro-id
;; ===========================
;; Dom -> Rec: Libro -> int
;; Nota: si por error llega '() (p. ej. no se encontró en RF09),
;; devolvemos '() para evitar crash. Si llega un Libro válido, retorna su ID.
(define (get-libro-id libro)
  (if (null? libro) '() (libro-id libro)))

