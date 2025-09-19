#lang racket
(provide
  crear-biblioteca
  biblioteca-libros biblioteca-usuarios biblioteca-prestamos
  biblioteca-max-libros-usuario biblioteca-dias-max-prestamo
  biblioteca-tasa-multa-diaria biblioteca-limite-deuda-max
  biblioteca-dias-max-retraso biblioteca-fecha-actual agregar-libro
  registrar-usuario obtener-usuario buscar-libro get-fecha libro-disponible?)

(require "tda-libro.rkt"
         "tda-usuario.rkt"
         racket/string)

;; Representación elegida (siguiendo la cabecera del RF5):
;; (list libros usuarios prestamos
;;       max-libros-usuario dias-max-prestamo tasa-multa-diaria
;;       limite-deuda-max dias-max-retraso fecha-actual)
;;
;; - libros:     lista de Libro
;; - usuarios:   lista de Usuario
;; - prestamos:  lista de Prestamo
;; - max-libros-usuario: int
;; - dias-max-prestamo:  int
;; - tasa-multa-diaria:  int
;; - limite-deuda-max:   int
;; - dias-max-retraso:   int
;; - fecha-actual:       string "DD/MM"

;; RF05: crear-biblioteca
(define (crear-biblioteca libros usuarios prestamos
                          max-libros-usuario
                          dias-max-prestamo
                          tasa-multa-diaria
                          limite-deuda-max
                          dias-max-retraso
                          fecha-inicial)
  (list libros usuarios prestamos
        max-libros-usuario
        dias-max-prestamo
        tasa-multa-diaria
        limite-deuda-max
        dias-max-retraso
        fecha-inicial))

;; Selectores
(define (biblioteca-libros biblio)               (list-ref biblio 0))
(define (biblioteca-usuarios biblio)             (list-ref biblio 1))
(define (biblioteca-prestamos biblio)            (list-ref biblio 2))
(define (biblioteca-max-libros-usuario biblio)   (list-ref biblio 3))
(define (biblioteca-dias-max-prestamo biblio)    (list-ref biblio 4))
(define (biblioteca-tasa-multa-diaria biblio)    (list-ref biblio 5))
(define (biblioteca-limite-deuda-max biblio)     (list-ref biblio 6))
(define (biblioteca-dias-max-retraso biblio)     (list-ref biblio 7))
(define (biblioteca-fecha-actual biblio)         (list-ref biblio 8))

;; ===========================
;; RF06 - agregar-libro
;; Dom -> Rec: Biblioteca x Libro -> Biblioteca
;; Si el ID ya existe en la biblioteca, retorna la biblioteca sin cambios.
;; Si no existe, inserta el libro (al inicio con cons).
;; ===========================
(define (agregar-libro biblio libro)
  (define id-nuevo (libro-id libro))
  (define existe?
    (ormap (lambda (lb) (= (libro-id lb) id-nuevo))
           (biblioteca-libros biblio)))
  (if existe?
      biblio
      (list (cons libro (biblioteca-libros biblio))
            (biblioteca-usuarios biblio)
            (biblioteca-prestamos biblio)
            (biblioteca-max-libros-usuario biblio)
            (biblioteca-dias-max-prestamo biblio)
            (biblioteca-tasa-multa-diaria biblio)
            (biblioteca-limite-deuda-max biblio)
            (biblioteca-dias-max-retraso biblio)
            (biblioteca-fecha-actual biblio))))

;; ===========================
;; RF07 - registrar-usuario
;; Dom -> Rec: Biblioteca x Usuario -> Biblioteca
;; Si el ID ya existe, retorna la misma biblioteca.
;; Si no existe, agrega al usuario al final (usando recursión natural).
;; ===========================
(define (registrar-usuario biblio usuario)
  (define id-nuevo (usuario-id usuario))

  ;; función recursiva auxiliar para insertar al final
  (define (insertar-final usuarios)
    (cond
      [(null? usuarios) (list usuario)]  ; caso base: lista vacía
      [else (cons (car usuarios) (insertar-final (cdr usuarios)))])) ; recursión natural

  ;; verificar si el usuario ya existe
  (define existe?
    (ormap (lambda (u) (= (usuario-id u) id-nuevo))
           (biblioteca-usuarios biblio)))

  (if existe?
      biblio
      (list (biblioteca-libros biblio)
            (insertar-final (biblioteca-usuarios biblio))
            (biblioteca-prestamos biblio)
            (biblioteca-max-libros-usuario biblio)
            (biblioteca-dias-max-prestamo biblio)
            (biblioteca-tasa-multa-diaria biblio)
            (biblioteca-limite-deuda-max biblio)
            (biblioteca-dias-max-retraso biblio)
            (biblioteca-fecha-actual biblio))))

;; ===========================
;; RF08 - obtener-usuario
;; Dom -> Rec: Biblioteca x int -> Usuario | '()
;; Busca por ID en la lista de usuarios. Si no lo encuentra, retorna '().
;; (Uso recursión natural para mantener el estilo del curso.)
;; ===========================
(define (obtener-usuario biblio id-buscado)
  (define (buscar lst)
    (cond
      [(null? lst) '()]                                   ; no encontrado
      [(= (usuario-id (car lst)) id-buscado) (car lst)]   ; encontrado
      [else (buscar (cdr lst))]))                          ; seguir buscando
  (buscar (biblioteca-usuarios biblio)))

;; ===========================
;; RF09 - buscar-libro
;; Dom -> Rec: Biblioteca x string x (string/int) -> Libro | '()
;; - criterio: "id" | "titulo" | "autor"
;; - valor:    int si criterio="id", string en caso contrario
;; ===========================
(define (buscar-libro biblio criterio valor)
  (define libros (biblioteca-libros biblio))

  (define (first-or-null lst)
    (if (null? lst) '() (car lst))) ; sobre colección, permitido

  (cond
    [(string-ci=? criterio "id")
     (first-or-null
      (filter (lambda (lb) (= (libro-id lb) valor)) libros))]

    [(string-ci=? criterio "titulo")
     (define q (string-downcase valor))
     (first-or-null
      (filter (lambda (lb)
                (string-contains? (libro-titulo lb) q))
              libros))]

    [(string-ci=? criterio "autor")
     (define q (string-downcase valor))
     (first-or-null
      (filter (lambda (lb)
                (string-contains? (libro-autor lb) q))
              libros))]

    [else '()]))

;; ===========================
;; RF11 - get-fecha
;; Dom -> Rec: Biblioteca -> string
;; Retorna la fecha actual del sistema.
;; ===========================
(define (get-fecha biblio)
  (biblioteca-fecha-actual biblio))

;; ===========================
;; RF12 - libro-disponible?
;; Dom -> Rec: Biblioteca x int -> Boolean
;; Retorna #t si el libro existe y su estado es "disponible".
;; Retorna #f si no existe o su estado no es disponible.
;; ===========================
(define (libro-disponible? biblio id-libro)
  (define lb (buscar-libro biblio "id" id-libro))  ; usa RF09
  (and (pair? lb)                                   ; existe (no es '())
       (string-ci=? (libro-estado lb) "disponible")))