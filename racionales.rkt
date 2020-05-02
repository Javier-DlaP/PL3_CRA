#lang scheme

(require "enteros.rkt")

;Enrique
(define escero_racional (lambda (x)
                          (esceroent(primero x))))
;(escero_racional ((par cero) uno))



(define mayor_racional(lambda (r)
                        (lambda (s)
                        ((esmayorent
                          ((prodent (primero r)) ((cocienteent ((mcment (segundo r)) (segundo s))) (segundo r))))
                         ((prodent (primero s)) ((cocienteent ((mcment (segundo r)) (segundo s))) (segundo s)))))))
;((mayor_racional ((par tres) cuatro)) ((par tres) cinco))

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