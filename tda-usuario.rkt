#lang racket
;; tda-usuario.rkt
;; -----------------------------------------------------------------------------
;; Usuario: id único, nombre, deuda acumulada, suspendido?, préstamos activos (ids).
;; Nota RF13: usuario-suspendido? evalúa el CAMPO 'suspendido. (No derivado)
;; -----------------------------------------------------------------------------
(require "util.rkt")
(provide make-usuario usuario? usuario-id usuario-nombre usuario-deuda
         usuario-suspendido? usuario-prestamos
         usuario-con-deuda usuario-con-estado usuario-agregar-prestamo usuario-remover-prestamo)

;; Representación: (list 'usuario id nombre deuda suspendido prestamos-activos)
(define (make-usuario id nombre)
  (list 'usuario (ensure-nat+ id 'id) nombre 0 #f '()))

(define (usuario? u) (and (pair? u) (eq? (car u) 'usuario)))
(define (usuario-id u)        (cadr u))
(define (usuario-nombre u)    (caddr u))
(define (usuario-deuda u)     (cadddr u))
(define (usuario-suspendido? u) (list-ref u 4))
(define (usuario-prestamos u) (list-ref u 5))

;; Modificadores puros
(define (usuario-con-deuda u nueva-deuda)
  (list 'usuario (usuario-id u) (usuario-nombre u) (ensure-nat nueva-deuda 'deuda) (usuario-suspendido? u) (usuario-prestamos u)))

(define (usuario-con-estado u nuevo-estado)
  (if (bool? nuevo-estado)
      (list 'usuario (usuario-id u) (usuario-nombre u) (usuario-deuda u) nuevo-estado (usuario-prestamos u))
      (error 'usuario-con-estado "nuevo-estado debe ser booleano")))

(define (usuario-agregar-prestamo u pid)
  (list 'usuario (usuario-id u) (usuario-nombre u) (usuario-deuda u) (usuario-suspendido? u)
        (cons (ensure-nat+ pid 'prestamo-id) (usuario-prestamos u))))

(define (usuario-remover-prestamo u pid)
  (define (keep xs)
    (cond [(null? xs) '()]
          [(= (car xs) pid) (keep (cdr xs))]
          [else (cons (car xs) (keep (cdr xs)))]))
  (list 'usuario (usuario-id u) (usuario-nombre u) (usuario-deuda u) (usuario-suspendido? u) (keep (usuario-prestamos u))))
