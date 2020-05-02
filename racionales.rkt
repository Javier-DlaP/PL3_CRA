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
                          (list (testenteros (primero x)) (testenteros (segundo x)))))

(define reducir_racional (lambda (x) ;Reduce la división que representa el número racional
                             ((par ((cocienteentaux (primero x)) ((mcdent (primero x)) (segundo x))))
                              ((cocienteentaux (segundo x)) ((mcdent (primero x)) (segundo x))))))

(define suma_racionales (lambda (x)
                          (lambda (y)
                            (reducir_racional ((par ((sument ((prodent ((cocienteentaux ((mcment (segundo x)) (segundo y))) (segundo x)))
                                                              (primero x)))
                                                     ((prodent ((cocienteentaux ((mcment (segundo x)) (segundo y))) (segundo y)))
                                                      (primero y))))
                                               ((mcment (segundo x)) (segundo y)))))))

(define resta_racionales (lambda (x)
                          (lambda (y)
                            (reducir_racional ((par ((restaent ((prodent ((cocienteentaux ((mcment (segundo x)) (segundo y))) (segundo x)))
                                                              (primero x)))
                                                     ((prodent ((cocienteentaux ((mcment (segundo x)) (segundo y))) (segundo y)))
                                                      (primero y))))
                                               ((mcment (segundo x)) (segundo y)))))))