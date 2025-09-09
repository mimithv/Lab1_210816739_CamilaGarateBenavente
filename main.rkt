#lang racket
;; main.rkt
;; -----------------------------------------------------------------------------
;; Re-exporta la interfaz pública de los TDAs para que los RFs posteriores
;; puedan trabajar SOLO con constructores/selectores/modificadores.
;; -----------------------------------------------------------------------------
(provide (all-from-out "util.rkt")
         (all-from-out "tda-fecha.rkt")
         (all-from-out "tda-config.rkt")
         (all-from-out "tda-libro.rkt")
         (all-from-out "tda-usuario.rkt")
         (all-from-out "tda-prestamo.rkt")
         (all-from-out "tda-biblioteca.rkt"))

(require "util.rkt"
         "tda-fecha.rkt"
         "tda-config.rkt"
         "tda-libro.rkt"
         "tda-usuario.rkt"
         "tda-prestamo.rkt"
         "tda-biblioteca.rkt")
