#lang racket
;; util.rkt
;; -----------------------------------------------------------------------------
;; Utilidades puramente funcionales (sin efectos colaterales).
;; Reglas del lab: lenguaje Scheme/Racket, funcional, sin bibliotecas externas.
;; -----------------------------------------------------------------------------
(provide str-low nat? nat+? ensure-nat ensure-nat+
         bool? id-eq? when-error)

;; str-low : string -> string
;; Descripción: normaliza a minúsculas.
;; Estrategia: directa, no recursiva.
;; Dom->Rec: string -> string
(define (str-low s) (string-downcase s))

;; Predicados numéricos (enteros de Racket).
(define (nat? x)   (and (integer? x) (>= x 0)))
(define (nat+? x)  (and (integer? x) (>= x 1)))

;; Aseguradores con error nombrado.
(define (ensure-nat v field)
  (if (nat? v) v (error 'ensure-nat (format "El campo ~a debe ser natural (>=0)" field))))

(define (ensure-nat+ v field)
  (if (nat+? v) v (error 'ensure-nat+ (format "El campo ~a debe ser natural positivo (>=1)" field))))

;; bool? : any -> boolean
(define (bool? b) (or (eq? b #t) (eq? b #f)))

;; id-eq? : any any -> boolean
;; Compara IDs (enteros) por igualdad.
(define (id-eq? a b) (and (integer? a) (integer? b) (= a b)))

;; when-error : symbol string -> (raises error)
(define (when-error who msg) (error who msg))
