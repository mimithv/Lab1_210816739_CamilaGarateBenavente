#lang racket

(require "main.rkt")

(display "===== BIBLIOTECA VIRTUAL - Script de Pruebas =====\n\n")

;; ===========================
;; RF02 - Crear usuarios
;; ===========================
(display ">> RF02: crear-usuario\n")
(define u01 (crear-usuario 1 "Jose"))
(define u02 (crear-usuario 2 "Carlos"))
(display "u01: ") (displayln u01)
(display "u02: ") (displayln u02)

;; ===========================
;; RF03 - Crear libros
;; ===========================
(display "\n>> RF03: crear-libro\n")
(define l101 (crear-libro 101 "El Hobbit" "J.R.R. Tolkien"))
(define l102 (crear-libro 102 "1984" "George Orwell"))
(display "l101: ") (displayln l101)
(display "l102: ") (displayln l102)

;; ===========================
;; RF04 - Crear préstamos
;; ===========================
(display "\n>> RF04: crear-prestamo\n")
(define p1001 (crear-prestamo 1001 (usuario-id u01) (libro-id l101) "01/01" 3))
(display "p1001: ") (displayln p1001)
(display "p1001 usuario-id: ") (displayln (prestamo-usuario p1001))
(display "p1001 libro-id:   ") (displayln (prestamo-libro p1001))
(display "p1001 fecha:      ") (displayln (prestamo-fecha p1001))
(display "p1001 dias:       ") (displayln (prestamo-dias p1001))

;; ===========================
;; RF05 - crear-biblioteca
;; ===========================
(display "\n>> RF05: crear-biblioteca\n")
(define b1
  (crear-biblioteca
    '() '() '()   ; libros, usuarios, prestamos
    2             ; max-libros-usuario
    3             ; dias-max-prestamo
    100           ; tasa-multa-diaria
    1000          ; limite-deuda-max
    1             ; dias-max-retraso
    "01/01"))     ; fecha-inicial
(display "b1: ") (displayln b1)

;; ===========================
;; RF06 - agregar-libro
;; ===========================
(display "\n>> RF06: agregar-libro\n")
(define b2 (agregar-libro b1 l101))
(define b3 (agregar-libro b2 l102))
(display "libros b1: ") (displayln (biblioteca-libros b1)) ; => '()
(display "libros b2: ") (displayln (biblioteca-libros b2)) ; => (l101)
(display "libros b3: ") (displayln (biblioteca-libros b3)) ; => (l102 l101)

;; Intento duplicado: NO debe cambiar
(define l101-dup (crear-libro 101 "EL HOBBIT" "J.R.R. TOLKIEN"))
(define b3-dup (agregar-libro b3 l101-dup))
(display "duplicado ID=101 → b3 == b3-dup: ")
(displayln (equal? (biblioteca-libros b3) (biblioteca-libros b3-dup))) ; => #t

;; ===========================
;; RF07 - registrar-usuario
;; ===========================
(display "\n>> RF07: registrar-usuario (al final de la lista)\n")
(define b4 (registrar-usuario b3 u01))
(define b5 (registrar-usuario b4 u02))
(display "usuarios b3: ") (displayln (biblioteca-usuarios b3)) ; => '()
(display "usuarios b4: ") (displayln (biblioteca-usuarios b4)) ; => (u01)
(display "usuarios b5: ") (displayln (biblioteca-usuarios b5)) ; => (u01 u02)

;; Duplicado: NO debe cambiar
(define b5-dup (registrar-usuario b5 u01))
(display "duplicado u01 → b5 == b5-dup: ")
(displayln (equal? (biblioteca-usuarios b5) (biblioteca-usuarios b5-dup))) ; => #t

;; ===========================
;; RF08 - obtener-usuario
;; ===========================
(display "\n>> RF08: obtener-usuario\n")

(display "Buscar ID=1 en b5: ")
(define u-encontrado (obtener-usuario b5 1))
(displayln u-encontrado)           ; => usuario con ID=1

(display "Buscar ID=99 en b5 (no existe): ")
(displayln (obtener-usuario b5 99)) ; => '()

;; ===========================
;; RF09 - buscar-libro
;; ===========================
(display "\n>> RF09: buscar-libro\n")

(display "Buscar por id=101: ")
(displayln (buscar-libro b3 "id" 101))          ; => l101

(display "Buscar por titulo contiene 'hob': ")
(displayln (buscar-libro b3 "titulo" "hob"))    ; => l101

(display "Buscar por autor contiene 'orwell': ")
(displayln (buscar-libro b3 "autor" "OrWeLl"))  ; => l102 (case-insensitive)

(display "Buscar inexistente (id=999): ")
(displayln (buscar-libro b3 "id" 999))          ; => '()

(display "Buscar por autor 'tolk' retorna primero si hay varios: ")
;; si tuvieras más de un libro con 'tolk', devolverá el primero
(displayln (buscar-libro b3 "autor" "tolk"))

;; ===========================
;; RF10 - get-libro-id
;; ===========================
(display "\n>> RF10: get-libro-id\n")

(display "ID desde buscar por autor 'tolkien': ")
(define libro-tolk (buscar-libro b3 "autor" "tolkien"))
(displayln (get-libro-id libro-tolk))  ; => 101 (según tus datos)

(display "ID cuando no existe (busqueda id=999): ")
(displayln (get-libro-id (buscar-libro b3 "id" 999))) ; => '() con versión segura

;; ===========================
;; RF11 - get-fecha
;; ===========================
(display "\n>> RF11: get-fecha\n")

(display "Fecha actual de b1: ")
(displayln (get-fecha b1)) ; debería mostrar "01/01"

;; ===========================
;; RF12 - libro-disponible?
;; ===========================
(display "\n>> RF12: libro-disponible?\n")

(display "¿Libro 101 disponible en b3? ")
(displayln (libro-disponible? b3 101)) ; => #t (si está como "disponible")

(display "¿Libro 999 disponible en b3? (no existe) ")
(displayln (libro-disponible? b3 999)) ; => #f

;; ===========================
;; RF14 - obtener-deuda
;; ===========================
(display "\n>> RF14: obtener-deuda\n")

;; Caso 1: usuario sin deuda
(define u14-a (crear-usuario 1401 "Luisa"))
(display "Deuda de u14-a (esperado 0): ")
(displayln (obtener-deuda u14-a))           ; => 0

;; Caso 2: usuario con deuda actualizada
(define u14-b (crear-usuario 1402 "Marco"))
(define u14-b$ (usuario-actualizar-deuda u14-b 200))
(display "Deuda de u14-b$ (esperado 200): ")
(displayln (obtener-deuda u14-b$))          ; => 200

;; (opcional) “aserciones” simples
(display "¿Deuda u14-a es 0? ") (displayln (= (obtener-deuda u14-a) 0))
(display "¿Deuda u14-b$ es 200? ") (displayln (= (obtener-deuda u14-b$) 200))


