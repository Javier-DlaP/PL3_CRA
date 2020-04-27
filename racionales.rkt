#lang scheme

(require "enteros.rkt")

;Enrique


;Javi


(define si (lambda (p x y) (p x y)))

(define esigual_racional (lambda (x y)
                           ((esigualent
                             ((prodent (primero x)) (segundo y)))
                             ((prodent (segundo x)) (primero y)))))