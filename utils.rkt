#lang racket

(provide (all-defined-out))

; The example below shows that "eval" is hidden inside the usage of a macro. When you think you
; have to call eval, please think of a macro instead !
(define-syntax show           ; look Ma, I can build my own tools ! Put it in your "utils.rkt" file
  (syntax-rules ()
    ((show expr) (begin (printf "? ~s\n" 'expr)        ; instead of ~a, the joker ~s keeps the Scheme syntax alive
                        (define res expr)              ; EVALuates expr (this "define" is more stylish than a let form)
                        (printf "--> ~s\n" res)))))

(define-syntax 
  while
  (syntax-rules ()
    (
     (while test e1 e2 ...)
     (let ()
       (define (iter)
         (if (not test)
             (void)
             (begin e1 e2 ... (iter))))
       (iter))
     )))

(define-syntax ++
  (syntax-rules ()
    (
     (++ i)
     (begin (set! i (+ i 1)) i)
     )))

(define-syntax +=
  (syntax-rules ()
    (
     (++ i nb)
     (begin (set! i (+ i nb)) i)
     )))

;---

(define-syntax loop
  (syntax-rules (For From To By Sum In When Collect)
    (
     (loop For i From a To b By s Sum expr)
     (local [
             (define i a)
             (define res 0)
             (define vb b)
             (define vs s)]
       (while (< i vb)
              (set! res (+ res expr))
              (set! i (+ i vs)))
       res))
    
    ((loop For x In L When test Collect expr)
     (local [(define res null)
             (define vL L)]
       (while (pair? vL)
              (define x (car vL))
              (when test
                  (set! res (cons x res)))
              (set! vL (cdr vL)))
       (reverse res)))))

(define-syntax loop2
  (syntax-rules (For From To By Sum In When Collect)
    (
     (loop2 For i From a To b By s Sum expr)
     (apply + (for/list ([i (in-range a b s)]) expr))
    )
    (
     (loop2 For x In L When test Collect expr)
     (for/list ([x (in-list L)] #:when test) expr))))
                  
