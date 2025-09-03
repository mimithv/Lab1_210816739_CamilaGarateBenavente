#lang racket
;; tda-prestamo.rkt
;; -----------------------------------------------------------------------------
;; Préstamo: id, id-usuario, id-libro, fecha-prestamo "DD/MM", dias-solicitados, estado.
;; RF15/RF16/RF17 operarán sobre estos campos.
;; -----------------------------------------------------------------------------
(require "util.rkt" "tda-fecha.rkt")
(provide make-prestamo prestamo? prestamo-id prestamo-uid prestamo-lid
         prestamo-fecha prestamo-dias prestamo-estado
         prestamo-con-estado)

;; estado: 'activo o 'completado (o 'devuelto)
(define (make-prestamo id uid lid fecha dias)
  (list 'prestamo (ensure-nat+ id 'id)
        (ensure-nat+ uid 'id-usuario)
        (ensure-nat+ lid 'id-libro)
        (make-fecha fecha)
        (ensure-nat+ dias 'dias-solicitados)
        'activo))

(define (prestamo? p) (and (pair? p) (eq? (car p) 'prestamo)))
(define (prestamo-id p)     (cadr p))
(define (prestamo-uid p)    (caddr p))
(define (prestamo-lid p)    (cadddr p))
(define (prestamo-fecha p)  (list-ref p 4))
(define (prestamo-dias p)   (list-ref p 5))
(define (prestamo-estado p) (list-ref p 6))

(define (prestamo-con-estado p nuevo-estado)
  (if (member nuevo-estado '(activo completado devuelto))
      (list 'prestamo (prestamo-id p) (prestamo-uid p) (prestamo-lid p)
            (prestamo-fecha p) (prestamo-dias p) nuevo-estado)
      (error 'prestamo-con-estado "Estado inválido")))
