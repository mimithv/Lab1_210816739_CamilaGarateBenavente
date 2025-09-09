#lang racket
;; tda-fecha.rkt
;; -----------------------------------------------------------------------------
;; Representa fechas "DD/MM" con meses de 30 días (no hay años).
;; Provee utilidades para sumar días y comparar.
;; -----------------------------------------------------------------------------
(provide make-fecha fecha? fecha-dia fecha-mes fecha->index index->fecha
         fecha+ dias-entre fecha<=? fecha<?)

;; Representación: string EXACTA "DD/MM" con 01<=DD<=30 y 01<=MM<=12.
;; Internamente ofrecemos funciones para convertir a índice [1..360].

;; make-fecha : string -> fecha
;; Valida formato "DD/MM" y rangos. Si es inválida, error.
(define (make-fecha s)
  (define parts (string-split s "/"))
  (if (and (= (length parts) 2)
           (andmap (lambda (x) (and (positive-integer-string? x) (<= (string->number x) 99))) parts))
      (let* ([dd (string->number (list-ref parts 0))]
             [mm (string->number (list-ref parts 1))])
        (if (and (<= 1 dd 30) (<= 1 mm 12))
            (format "~a/~a"
                    (~r dd #:min-width 2 #:pad-string "0")
                    (~r mm #:min-width 2 #:pad-string "0"))
            (error 'make-fecha "Fecha fuera de rango DD/MM")))
      (error 'make-fecha "Formato inválido, use \"DD/MM\"")))

;; fecha? : any -> boolean
(define (fecha? s)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (equal? s (make-fecha s))))

;; Helpers
(define (positive-integer-string? x)
  (and (regexp-match #rx"^[0-9]+$" x) #t))

;; fecha->index : fecha -> nat+ (1..360)
(define (fecha->index f)
  (let* ([parts (string-split f "/")]
         [dd (string->number (first parts))]
         [mm (string->number (second parts))])
    (+ (* (- mm 1) 30) dd)))

;; index->fecha : nat+ -> fecha
(define (index->fecha k)
  (define k2 (let ([m (modulo k 360)]) (if (= m 0) 360 m)))
  (define mm (quotient (- k2 1) 30))
  (define dd (modulo (- k2 1) 30))
  (make-fecha (format "~a/~a"
                      (~r (+ dd 1) #:min-width 2 #:pad-string "0")
                      (~r (+ mm 1) #:min-width 2 #:pad-string "0"))))

;; fecha+ : fecha nat -> fecha
;; Suma n días (n>=0), con rollover mensual (cada mes 30 días).
(define (fecha+ f n)
  (index->fecha (+ (fecha->index f) n)))

;; dias-entre : fecha fecha -> nat
;; Si f2 >= f1 retorna diferencia, si f2 < f1 asume que avanzó el calendario (giro).
(define (dias-entre f1 f2)
  (define i1 (fecha->index f1))
  (define i2 (fecha->index f2))
  (if (<= i1 i2) (- i2 i1) (+ (- 360 i1) i2)))

;; Comparadores
(define (fecha<=? a b) (<= (fecha->index a) (fecha->index b)))
(define (fecha<? a b)  (<  (fecha->index a) (fecha->index b)))

;; Selectores de conveniencia
(define (fecha-dia f) (string->number (first (string-split f "/"))))
(define (fecha-mes f) (string->number (second (string-split f "/"))))
