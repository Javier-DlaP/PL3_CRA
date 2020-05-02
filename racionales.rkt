#lang scheme

(require "enteros.rkt")

;Enrique
(define escero_racional (lambda (x)
                          (esceroent(primero x))))
;(escero_racional ((par cero) uno))

;Javi


(define si (lambda (p x y) (p x y)))

(define esigual_racional (lambda (x y)
                           ((esigualent
                             ((prodent (primero x)) (segundo y)))
                             ((prodent (segundo x)) (primero y)))))

(define test_racionales (lambda (x)
                          (display "(")
                          (display (testenteros (primero x)))
                          (display " ")
                          (display (testenteros (segundo x)))
                          (display ")")))