#lang racket

(require graphics/graphics)    ; le graphisme simplifie de la librairie "graphics"
(open-graphics) ; initialisations de la librairie "graphics"

(provide turtle% TURTLES)

(define TURTLES '())
(define SIZE 500)
(define twin (open-viewport "Turtlegraphics" SIZE SIZE))

(define turtle%
  (class object%
    (define SIZE 500)
    (define SIZE/2 (/ SIZE 2))
    
    (define (%turtlepoint->posn x y)
      (make-posn (+ SIZE/2 x) (- SIZE/2 y)))   ; repere au centre, unite 1 pixel
    (define tr-segment (draw-line twin))
    (define PENDOWN? #t)     ; mode pen down initial
    (define CAP 0)           ; en degres, 0 = Nord, 90 = Est
    (define XCOR 0)
    (define YCOR 0)
    (define PI/180 (/ pi 180))
    (define 180/PI (/ 180 pi))
    (define (real-mod360 x)                     ; x reel modulo 360
      (- x (* 360 (floor (/ x 360.0)))))
    
    (define/public (init pos cap [reset? #t])     ; Exemple : (init '(-100 50) 90) cap vers l'Est
      (when reset? ((clear-viewport twin)))
      (pen-up)
      (set-position pos)
      (set-heading cap)
      (pen-down))
    
    (define/public (pen-up)
      (set! PENDOWN? #f))
    
    (define/public (pen-down)
      (set! PENDOWN? #t))
    
    (define/public (set-heading angle-en-degres)
      (set! CAP (real-mod360 angle-en-degres)))
    
    (define/public (heading)
      CAP)
    
    (define/public (set-position L)    ; L == (x y) est un turtlepoint
      (let ((x (car L)) (y (cadr L)))
        (when PENDOWN? 
          (tr-segment (%turtlepoint->posn XCOR YCOR) (%turtlepoint->posn x y)))
        (set! XCOR x)
        (set! YCOR y)))
    
    (define/public (position)
      (list XCOR YCOR))
    
    (define/public (xcor)
      (car (position)))
    
    (define/public (ycor)
      (cadr (position)))
    
    (define/public (right angle-en-degres)
      (set-heading (+ CAP angle-en-degres)))
    
    (define/public (left angle-en-degres)
      (set-heading (- CAP angle-en-degres)))
    
    (define/public (forward dist)
      (set-position (list (+ XCOR (* dist (sin (degrees->radians CAP))))
                          (+ YCOR (* dist (cos (degrees->radians CAP)))))))
    
    (define/public (back dist)
      (forward (- dist)))
    
    (define/public (toward L)    ; force la tortue a regarder vers le point L = (x y)
      (let ((x (car L)) (y (cadr L)))
        (let ((dx (- XCOR x)) (dy (- YCOR y)) (angle '?))
          (if (= dx 0) 
              (set! angle (if (> dy 0) -90 90))
              (set! angle (radians->degrees (atan (/ dy dx)))))
          (set! angle (- angle))
          (when (> dx 0) (set! angle (+ angle 180)))  ; atant defini entre 0 et 180
          (set-heading (+ angle 90)))))
    
    (define-syntax repeat            ; la boucle (repeat n e1 e2 ...), avec n entier > 0
      (syntax-rules ()
        ((repeat n e1 e2 ...) (do ((i 0 (+ i 1))) ((= i n)) e1 e2 ...))))
    
    
    (set! TURTLES (cons this TURTLES))
    (super-new)))
