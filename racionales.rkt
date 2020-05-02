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


(define inversa? (lambda (x)
                   (neg (escero_racional (determinante x)))))
;(inversa? matriz_prueba1)
;#<procedure:true>
;(inversa? matriz_prueba2)
;#<procedure:false>

(define transpuesta (lambda (x)
                      ((((definir_matriz (primero(primero x))) (primero (segundo x))) (segundo (primero x))) (segundo (segundo x)))))

(define adjunta (lambda (x)
                  ((((definir_matriz(segundo(segundo (transpuesta x))))
                     ((par (opuesto(primero(primero(segundo (transpuesta x))))))(segundo(primero(segundo (transpuesta x))))))
                    ((par (opuesto(primero(segundo (primero (transpuesta x)))))) (segundo(segundo(primero (transpuesta x))))))
                   (primero (primero (transpuesta x))))))

(define inversa (lambda (x)
                  ((((definir_matriz
                       ((prod_racionales(primero(primero (adjunta x)))) (inverso_racionales (determinante x))))
                     ((prod_racionales(segundo(primero (adjunta x)))) (inverso_racionales (determinante x))))
                    ((prod_racionales(primero(segundo (adjunta x)))) (inverso_racionales (determinante x))))
                   ((prod_racionales(segundo(segundo (adjunta x)))) (inverso_racionales (determinante x))))))
                       

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

(define prod_racionales (lambda (x)
                          (lambda (y)
                            (reducir_racional ((par ((prodent (primero x)) (primero y)))
                                               ((prodent (segundo x)) (segundo y)))))))

(define inverso_racionales (lambda (x)
                             ((par (segundo x)) (primero x))))

(define determinante (lambda (x)
                       ((resta_racionales
                         ((prod_racionales
                           (primero (primero x)))
                          (segundo (segundo x))))
                         ((prod_racionales
                           (primero (segundo x)))
                          (segundo (primero x))))))

(define suma_matrices (lambda (x)
                        (lambda (y)
                          ((((definir_matriz
                               (reducir_racional ((suma_racionales (primero (primero x))) (primero (primero y)))))
                             (reducir_racional ((suma_racionales (segundo (primero x))) (segundo (primero y)))))
                            (reducir_racional ((suma_racionales (primero (segundo x))) (primero (segundo y)))))
                           (reducir_racional ((suma_racionales (segundo (segundo x))) (segundo (segundo y))))))))

(define prod_matrices (lambda (x)
                        (lambda (y)
                          ((((definir_matriz
                               (reducir_racional ((suma_racionales
                                                   ((prod_racionales (primero (primero x))) (primero (primero y))))
                                                  ((prod_racionales (segundo (primero x))) (primero (segundo y))))))
                             (reducir_racional ((suma_racionales
                                                   ((prod_racionales (primero (primero x))) (segundo (primero y))))
                                                  ((prod_racionales (segundo (primero x))) (segundo (segundo y))))))
                            (reducir_racional ((suma_racionales
                                                   ((prod_racionales (primero (segundo x))) (primero (primero y))))
                                                  ((prod_racionales (segundo (segundo x))) (primero (segundo y))))))
                           (reducir_racional ((suma_racionales
                                                   ((prod_racionales (primero (segundo x))) (segundo (primero y))))
                                                  ((prod_racionales (segundo (segundo x))) (segundo (segundo y)))))))))

;Código copiado

(define definir_matriz (lambda (a)
                         (lambda (b)
                           (lambda (c)
                             (lambda (d)
                               ((par ((par a) b)) ((par c) d)))))))


(define test_matriz (lambda (m)
                        (list (list (test_racionales (primero (primero m))) (test_racionales (segundo (primero m))))
                              (list (test_racionales (primero (segundo m))) (test_racionales (segundo (segundo m))))
                        )
                      )
)

(define identidad ((((definir_matriz ((par uno) uno)) ((par cero) uno)) ((par cero) uno)) ((par uno) uno)))
(define matriz_nula ((((definir_matriz ((par cero) uno)) ((par cero) uno)) ((par cero) uno)) ((par cero) uno)))
(define matriz_prueba1 ((((definir_matriz ((par dos) cuatro)) ((par cuatro) cuatro)) ((par -uno) cuatro)) ((par cinco) cuatro)))
(define matriz_prueba2 ((((definir_matriz ((par uno) cuatro))   ((par -cuatro) seis))     ((par dos) ocho)) ((par -dos) tres)))
(define matriz_prueba3 ((((definir_matriz ((par uno) dos))   ((par -cuatro) dos))     ((par dos) dos)) ((par -tres) dos)))