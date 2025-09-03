#lang racket

;; pruebas_210816739_Garate_Benavente.rkt
;; -------------------------------------------------------------------
;; Script de pruebas para RF01 (TDAs) - Biblioteca Virtual
;; Autor: 210816739 - Garate Benavente
;; Paradigma: Funcional (sin mutación), sin bibliotecas externas.
;; Cómo usar:
;;   1) Coloca este archivo en la MISMA carpeta que main.rkt y los TDAs.
;;   2) Abre este archivo en DrRacket y presiona "Run" (▶).
;;   3) Revisa la consola: todas las líneas con [OK] deberían pasar.
;;   4) Si ves [FAIL], lee el mensaje para identificar qué no coincide.
;;
;; Nota: Los profesores revisan con un script similar; este archivo
;; está estructurado para que puedan observar creación/uso básico
;; de cada TDA exigido en RF01.
;; -------------------------------------------------------------------

(require "main.rkt")

;; Utilidad local de pruebas (sin librerías externas)
;; check-equal : string any any -> void
;; Muestra [OK] si (equal? got expected), o [FAIL] con detalles.
(define (check-equal label got expected)
  (if (equal? got expected)
      (displayln (format "~a: [OK]" label))
      (displayln (format "~a: [FAIL] got=~a expected=~a" label got expected))))

;; =============================
;; 1) TDA Fecha
;; =============================
(displayln "=== TDA Fecha ===")
(define f1 (make-fecha "01/01"))
(define f2 (make-fecha "30/01"))
(define f3 (fecha+ "30/01" 1)) ; rollover a "01/02"

(check-equal "make-fecha formatea 01/01" f1 "01/01")
(check-equal "dias-entre 01/01->30/01" (dias-entre f1 f2) 29)
(check-equal "fecha+ 30/01 + 1 = 01/02" f3 "01/02")
(check-equal "fecha<=? 01/01 <= 30/01" (fecha<=? f1 f2) #t)
(check-equal "fecha<? 30/01 <? 01/01 (giro no es menor)" (fecha<? f2 f1) #f)

(newline)

;; =============================
;; 2) TDA Config
;; =============================
(displayln "=== TDA Config ===")
(define cfg (make-config 2 3 100 1000 1 "01/01"))

(check-equal "cfg-max-libros" (cfg-max-libros cfg) 2)
(check-equal "cfg-dias-max"   (cfg-dias-max cfg)   3)
(check-equal "cfg-tasa-multa" (cfg-tasa-multa cfg) 100)
(check-equal "cfg-limite-deuda" (cfg-limite-deuda cfg) 1000)
(check-equal "cfg-max-retraso"  (cfg-max-retraso cfg) 1)
(check-equal "cfg-fecha-inicial" (cfg-fecha-inicial cfg) "01/01")

(newline)

;; =============================
;; 3) TDA Libro
;; =============================
(displayln "=== TDA Libro ===")
(define libro1 (make-libro 101 "El Hobbit" "J.R.R. Tolkien"))

(check-equal "libro-id" (libro-id libro1) 101)
(check-equal "libro-titulo normalizado" (libro-titulo libro1) "el hobbit")
(check-equal "libro-autor normalizado"  (libro-autor libro1)  "j.r.r. tolkien")
(check-equal "libro-disponible? inicial" (libro-disponible? libro1) #t)
(check-equal "marcar-no-disponible" (libro-disponible? (libro-marcar-no-disponible libro1)) #f)
(check-equal "marcar-disponible"    (libro-disponible? (libro-marcar-disponible (libro-marcar-no-disponible libro1))) #t)

(newline)

;; =============================
;; 4) TDA Usuario
;; =============================
(displayln "=== TDA Usuario ===")
(define user1 (make-usuario 1 "Jose"))

(check-equal "usuario-id" (usuario-id user1) 1)
(check-equal "usuario-nombre" (usuario-nombre user1) "Jose")
(check-equal "usuario-deuda inicial 0" (usuario-deuda user1) 0)
(check-equal "usuario-suspendido? inicial #f" (usuario-suspendido? user1) #f)

(define user1-con-deuda (usuario-con-deuda user1 200))
(check-equal "usuario-con-deuda 200" (usuario-deuda user1-con-deuda) 200)

(define user1-susp (usuario-con-estado user1-con-deuda #t))
(check-equal "usuario-con-estado #t" (usuario-suspendido? user1-susp) #t)

(define user1-pre (usuario-agregar-prestamo user1-susp 1001))
(check-equal "usuario-agregar-prestamo agrega id" (member 1001 (usuario-prestamos user1-pre)) '(1001))

(define user1-pre-removed (usuario-remover-prestamo user1-pre 1001))
(check-equal "usuario-remover-prestamo quita id" (member 1001 (usuario-prestamos user1-pre-removed)) #f)

(newline)

;; =============================
;; 5) TDA Prestamo
;; =============================
(displayln "=== TDA Prestamo ===")
(define p1 (make-prestamo 1001 1 101 "01/01" 3))

(check-equal "prestamo-id" (prestamo-id p1) 1001)
(check-equal "prestamo-uid" (prestamo-uid p1) 1)
(check-equal "prestamo-lid" (prestamo-lid p1) 101)
(check-equal "prestamo-fecha" (prestamo-fecha p1) "01/01")
(check-equal "prestamo-dias" (prestamo-dias p1) 3)
(check-equal "prestamo-estado inicial" (prestamo-estado p1) 'activo)
(check-equal "prestamo-con-estado devuelto" (prestamo-estado (prestamo-con-estado p1 'devuelto)) 'devuelto)

(newline)

;; =============================
;; 6) TDA Biblioteca
;; =============================
(displayln "=== TDA Biblioteca ===")
(define bib1 (make-biblioteca "01/01" cfg (list libro1) (list user1) (list p1)))

(check-equal "bib-fecha" (bib-fecha bib1) "01/01")
(check-equal "bib-config -> dias-max" (cfg-dias-max (bib-config bib1)) 3)
(check-equal "bib-libros cantidad" (length (bib-libros bib1)) 1)
(check-equal "bib-usuarios cantidad" (length (bib-usuarios bib1)) 1)
(check-equal "bib-prestamos cantidad" (length (bib-prestamos bib1)) 1)

;; Modificador puro de fecha
(define bib2 (bib-con-fecha bib1 "02/01"))
(check-equal "bib-con-fecha cambia solo fecha" (and (equal? (bib-fecha bib2) "02/01")
                                                    (equal? (length (bib-libros bib2)) 1)
                                                    (equal? (length (bib-usuarios bib2)) 1)
                                                    (equal? (length (bib-prestamos bib2)) 1))
             #t)

(displayln "=== FIN RF01 ===")
