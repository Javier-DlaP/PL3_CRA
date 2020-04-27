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


