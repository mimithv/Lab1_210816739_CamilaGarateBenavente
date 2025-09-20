#lang racket

(require racket/string
         racket/format)

(provide crear-prestamo
         prestamo-id prestamo-usuario prestamo-libro
         prestamo-fecha prestamo-dias obtener-fecha-vencimiento
         calcular-dias-retraso)

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

;; ===========================
;; RF15 - obtener-fecha-vencimiento
;; Dom -> Rec: Prestamo -> string ("DD/MM")
;; Suma diasSolicitados a fechaPrestamo ("DD/MM").
;; Todos los meses tienen 30 días; si pasa de 30, avanza mes (12->01).
;; ===========================
(define (obtener-fecha-vencimiento prestamo)
  (define fecha (prestamo-fecha prestamo))   ; "DD/MM"
  (define dias  (prestamo-dias prestamo))    ; int

  ;; separar "DD/MM"
  (define partes (regexp-split #px"/" fecha))
  (define D (string->number (first partes)))
  (define M (string->number (second partes)))

  ;; normalizar con meses de 30 días (recursión)
  (define (normalizar d m)
    (if (<= d 30)
        (values d m)
        (normalizar (- d 30) (if (= m 12) 1 (+ m 1)))))

  (define-values (finalD finalM) (normalizar (+ D dias) M))

  ;; helper para 2 dígitos
  (define (pad2 n)
    (if (< n 10) (string-append "0" (number->string n))
                 (number->string n)))

  (string-append (pad2 finalD) "/" (pad2 finalM)))

;; ===========================
;; RF16 - calcular-dias-retraso
;; Dom -> Rec: string("DD/MM") x string("DD/MM") -> int
;; Retorna 0 si fecha-actual <= fecha-venc. Meses de 30 días.
;; ===========================
(define (calcular-dias-retraso fecha-venc fecha-actual)
  ;; parse "DD/MM" -> (values D M)
  (define (parse-fecha s)
    (define partes (regexp-split #px"/" s))
    (values (string->number (first partes))
            (string->number (second partes))))
  ;; convierte a día absoluto (día 1..360) sin años: (mes-1)*30 + día
  (define (to-abs d m) (+ d (* 30 (- m 1))))

  (define-values (dv mv) (parse-fecha fecha-venc))
  (define-values (da ma) (parse-fecha fecha-actual))

  (define diff (- (to-abs da ma) (to-abs dv mv)))
  (if (> diff 0) diff 0))

