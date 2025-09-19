#lang racket

(provide crear-usuario
         usuario-id usuario-nombre usuario-deuda
         usuario-suspendido? usuario-libros
         usuario-actualizar-deuda usuario-suspender usuario-activar
         usuario-agregar-libro usuario-remover-libro usuario-estado
         usuario-actualizar-estado obtener-deuda)
;; ==========================================
;; TDA Usuario
;; ==========================================
;; Representación: (list ID nombre deuda estado librosPrestados)
;; - ID: número o string único
;; - nombre: string
;; - deuda: número (inicia en 0)
;; - estado: string ("activo" o "suspendido")
;; - librosPrestados: lista de IDs de libros

;; Representación (actualizada para RF13):
;; (list ID nombre deuda suspendido? librosPrestados)
;; - suspendido? : #t si está suspendido, #f si está activo

;; ==========================================
;; RF02 - crear-usuario
;; ==========================================
;; Crea un usuario con deuda inicial 0, estado "activo" y sin libros prestados.
;; Dom -> Rec: (int x string) -> Usuario
;; Cabecera: (crear-usuario id nombre)

(define (crear-usuario id nombre)
  (list id nombre 0 #f '()))          ; inicia activo (#f)

;; ==========================================
;; Selectores
;; ==========================================

(define (usuario-id u)            (list-ref u 0))
(define (usuario-nombre u)        (list-ref u 1))
(define (usuario-deuda u)         (list-ref u 2))
(define (usuario-suspendido? u)   (list-ref u 3))   ; <-- RF13
(define (usuario-libros u)        (list-ref u 4))

;; ==========================================
;; Modificadores
;; ==========================================

(define (usuario-actualizar-deuda u nueva)
  (list (usuario-id u) (usuario-nombre u) nueva (usuario-suspendido? u) (usuario-libros u)))

(define (usuario-suspender u)
  (list (usuario-id u) (usuario-nombre u) (usuario-deuda u) #t (usuario-libros u)))

(define (usuario-activar u)
  (list (usuario-id u) (usuario-nombre u) (usuario-deuda u) #f (usuario-libros u)))

(define (usuario-agregar-libro u libroID)
  (list (usuario-id u) (usuario-nombre u) (usuario-deuda u) (usuario-suspendido? u)
        (cons libroID (usuario-libros u))))

(define (usuario-remover-libro u libroID)
  (list (usuario-id u) (usuario-nombre u) (usuario-deuda u) (usuario-suspendido? u)
        (remove libroID (usuario-libros u))))

;; ==========================
;; Compatibilidad con versión previa basada en strings
;; (por si tu script ya usa estos nombres)
;; ==========================
(define (usuario-estado u) (if (usuario-suspendido? u) "suspendido" "activo"))

(define (usuario-actualizar-estado u nuevoEstado)
  (if (string-ci=? nuevoEstado "suspendido")
      (usuario-suspender u)
      (usuario-activar u)))

;; ===========================
;; RF14 - obtener-deuda
;; Dom -> Rec: Usuario -> int
;; Retorna la deuda acumulada del usuario.
;; ===========================
(define (obtener-deuda usuario)
  (usuario-deuda usuario))
