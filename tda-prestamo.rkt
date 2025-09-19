#lang racket

(provide crear-prestamo
         prestamo-id prestamo-usuario prestamo-libro
         prestamo-fecha prestamo-dias)

;; ==========================================
;; TDA Prestamo
;; ==========================================
;; Representación: (list id idUsuario idLibro fechaPrestamo diasSolicitados)
;; - id: número único
;; - idUsuario: referencia al usuario
;; - idLibro: referencia al libro
;; - fechaPrestamo: string "DD/MM"
;; - diasSolicitados: número entero (máx definido por sistema)

;; ==========================================
;; RF04 - crear-prestamo
;; ==========================================
;; Dom -> Rec: (int x int x int x string x int) -> Prestamo

(define (crear-prestamo id idUsuario idLibro fechaPrestamo diasSolicitados)
  (list id idUsuario idLibro fechaPrestamo diasSolicitados))

;; ==========================================
;; Selectores
;; ==========================================

(define (prestamo-id prestamo) (list-ref prestamo 0))
(define (prestamo-usuario prestamo) (list-ref prestamo 1))
(define (prestamo-libro prestamo) (list-ref prestamo 2))
(define (prestamo-fecha prestamo) (list-ref prestamo 3))
(define (prestamo-dias prestamo) (list-ref prestamo 4))
