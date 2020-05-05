#lang scheme

(require "enteros.rkt")

;Enrique

(define escero_racional (lambda (x) ;racional
                          (esceroent(primero x))))  ;Comprobamos que el numerador sea 0



(define mayor_racional(lambda (r) ;racional 1
                        (lambda (s) ;racional 2
                          ((esmayorent  ;comprobamos que el primer elemento es mayor que el segundo
                            ((prodent (primero r)) ((cocienteent ((mcment (segundo r)) (segundo s))) (segundo r))))  ;Igualamos denominadores de racionales y actualizamos numeradores
                           ((prodent (primero s)) ((cocienteent ((mcment (segundo r)) (segundo s))) (segundo s)))))))

(define inversa? (lambda (x) ;matriz
                   (neg (escero_racional (determinante x))))) ;una matriz tiene inversa si su determinante no es 0

(define transpuesta (lambda (x) ;matriz
                      ((((definir_matriz (primero(primero x))) (primero (segundo x))) (segundo (primero x))) (segundo (segundo x))))) ;transpuesta de matriz 2x2, cambiando racionales de la diagonal secundatia

(define adjunta (lambda (x) ;matriz
                  ((((definir_matriz(segundo(segundo (transpuesta x)))) ;generamos nueva matriz con los elementos de la adjunta
                     ((par (opuesto(primero(primero(segundo (transpuesta x))))))(segundo(primero(segundo (transpuesta x)))))) ; (-1)^(1+2)->Impar-> realizamos opuesto
                    ((par (opuesto(primero(segundo (primero (transpuesta x)))))) (segundo(segundo(primero (transpuesta x)))))) ; (-1)^(2+1)->Impar-> realizamos opuesto
                   (primero (primero (transpuesta x))))))

(define inversa (lambda (x) ;matriz
                  ((((definir_matriz ;genramos nueva matriz con el resultado de la adjunta entre el determinante
                       (reducir_racional((prod_racionales(primero(primero (adjunta x)))) (inverso_racionales (determinante x))))) ;para realizar la división invertimos el racional determinante y multiplicamos
                     (reducir_racional((prod_racionales(segundo(primero (adjunta x)))) (inverso_racionales (determinante x)))))
                    (reducir_racional((prod_racionales(primero(segundo (adjunta x)))) (inverso_racionales (determinante x)))))
                   (reducir_racional((prod_racionales(segundo(segundo (adjunta x)))) (inverso_racionales (determinante x)))))))

(define rango (lambda (x) ;matriz
                ((escero_racional (determinante x)) uno dos))) ; en una matriz 2x2 si el denominador es 0, rango 0, sino el rango es 2

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

(define cuadrado_matrices (lambda (x)
                            ((prod_matrices x) x)))

(define potencia_matrices (lambda (x)
                            (lambda (y)
                              (((Y (lambda (f)
                                    (lambda (a)
                                      (lambda (b)
                                        ((((esigualnat b) un)
                                         (lambda (no_use) a)
                                         (((esigualnat ((restonat b) deux)) zero)
                                          (lambda (no_use) (cuadrado_matrices ((f a) ((cocientenat b) deux))))
                                          (lambda (no_use) ((prod_matrices a) ((f a) ((restanat b) un)))))) zero)))))x) y))))
                                            

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
