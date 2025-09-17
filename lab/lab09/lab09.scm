(define (over-or-under num1 num2)
    (cond
        ((= num1 num2) 0)
        ((> num1 num2) 1)
        ((< num1 num2) -1)
    )
)

(define (make-adder num) 
    (lambda (inc) (+ inc num))
)

(define (composed f g) 
    (lambda (x) (f (g x)))
)

(define (repeat f n) 
    (lambda (x)
        (define (rec k now)
            (
                if (zero? k)
                    now
                    (rec (- k 1) (f now))
            )
        )
        (rec n x)
    )
)
    
(define (max a b)
  (if (> a b)
      a
      b))

(define (min a b)
  (if (> a b)
      b
      a))

(define (gcd a b) 
    (let
        (
        (c (max a b))
        (d (min a b))
        )
        (if (zero? (modulo c d))
            d
            (gcd d (modulo c d))
        )
    )
)
