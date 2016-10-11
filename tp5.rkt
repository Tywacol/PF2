#lang racket

; TP5
(require "utils.rkt" "adt-turtle.rkt" "adt-turtle-hard.rkt")

; Ex1
(define (protected-value x passwd)
  (let ((p passwd) (v x))
    (define (this methode . Largs)
      (case methode
        ((getValue) v)
        ((setValue) (begin (printf "Gimme a password : ")
                           (define inputPswd (read))
                           (if (equal? p inputPswd)
                               (set! v (car Largs))
                               (printf "Acces denied !"))))
        (else (error "Methode inconnue" methode))))
    this))

; Ex2
;(define (equi c)
;  (repeat 3
;          (forward c)
;          (left 120)))
;
;(define (poly5) ; les sommets sont les solutions de z 5 = 100 dans le plan complexe
;  (define a (* 2/5 pi)) ; la boucle de tracé des 5 segments
;  (for ([k (in-range 1 6)])
;    (set-position (list (* 100 (cos (* k a))) (* 100 (sin (* k a)))))))
;(init '(100 0) 0) ; on se place sur le premier sommet, cap Nord
;(poly5)

; Ex2b)
(define t1 (new turtle%))
(define t2 (new turtle%))
(define (rd-angl) (random 360))
(send t1 init '(0 0) (rd-angl))
(send t2 init '(-200 200) 0)


(define (distance t1 t2)
  (define p1 (send t1 position))
  (define p2 (send t2 position))
  (sqrt (+ (sqr (- (car p1) (car p2))) (sqr (- (cadr p1) (cadr p2))))))

(define (is-out? t)
  (define p (send t position))
  (or (< (car p) -250) (< (cadr p) -250) (> (car p) 250) (> (cadr p) 250)))
 
  

(define (poursuite)
  (define (rd-angl) (random 360))
  (define t1 (new turtle%))
  (define t2 (new turtle%))
  (send t1 init '(0 0) (rd-angl))
  (send t2 init '(-200 200) 0)
  (while (and (not (<= (distance t1 t2) 2)) (not (is-out? t1)))
         (sleep 0.008)
         (send t1 set-heading (rd-angl))
         (send t1 forward 2)
         (send t2 toward (send t1 position))
         (send t2 forward 1)))
         
(poursuite)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  