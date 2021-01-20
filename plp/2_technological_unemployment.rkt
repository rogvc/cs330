#lang plait

; Design Recipe:
;   1- Determine how the information involved is encoded as data
;       - 
;       - 
;   2- Describe what your program should do in prose, giving it a name
;
;   3- Write down the contract of your program
;       - What it assumes about inputs
;       - What it guarantees about outputs
;   4- Write down examples
;
;   5- Write down the template
;   6- Fill up that template

; Local Supermarket:
;   1- Determine how the information involved is encoded as data
;       - pennies : Number
;       - nickels : Number
;       - dimes : Number
;       - quarters : Number
;       - penny_value : Number
;       - nickel_value : Number
;       - dime_value : Number
;       - quarter_value : Number

;   2- Describe what your program should do in prose, giving it a name
;       - coin-summation should get an amount of distinct coins,
;           and return their total value. 

;   3- Write down the contract of your program
;       - What it assumes about inputs
;       - What it guarantees about outputs

(define-type-alias pennies Number)
(define-type-alias nickels Number)
(define-type-alias dimes Number)
(define-type-alias quarters Number)
(sum-coins : (Number Number Number Number -> Number))
;   5- Write down the template
;   6- Fill up that template
(define penny_value 1)
(define nickel_value 5)
(define dime_value 10)
(define quarter_value 25)
(define (sum-coins pennies nickels dimes quarters)
    (+ (* quarters quarter_value) (+ (* dimes dime_value) (+ (* nickels nickel_value) (* pennies penny_value))))
)

;   4- Write down examples
(test (sum-coins 1 1 1 1) 41)
(test (sum-coins 1 0 0 0) 1)
(test (sum-coins 0 0 0 1) 25)
(test (sum-coins 0 0 0 4) 100)

; Area Pipe:
;   1- Determine how the information involved is encoded as data
;       - inner-radius : Number
;       - wall-thickness : Number
;       - length : Number
;       - pi : Number
;   2- Describe what your program should do in prose, giving it a name
;       - area-piper calculates the surface area of an open cylinder
;   3- Write down the contract of your program
;       - What it assumes about inputs
;       - What it guarantees about outputs

(define-type-alias inner-radius Number)
(define-type-alias wall-thickness Number)
(define-type-alias length Number)
(area-pipe : (Number Number Number -> Number))
;   5- Write down the template
;   6- Fill up that template
(define (area-pipe inner-radius wall-thickness length)
    (let ([outer-radius (+ inner-radius wall-thickness)])
        (* (* 2 3.1415926535897) (* (+ inner-radius outer-radius) (+ (- outer-radius inner-radius) length)))
    )
)
;   4- Write down examples
(test (area-pipe 1 1 1) 37.699)
(test (area-pipe 5 2 3) 376.991)
(test (area-pipe 4 12 3) 1884.955)

; Ratio Circumference:
;   1- Determine how the information involved is encoded as data
;       - pi : Number
;   2- Describe what your program should do in prose, giving it a name
;       - PiProvider returns an approximation of the value of the ratio of 
;           the circumference of a circle to its diameter - AKA pi.
;   3- Write down the contract of your program
;       - No inputs, the result is a constant
(pi-provider : Number)
;   5- Write down the template
;   6- Fill up that template
(define pi-provider
    3.1415926535897
)
;   4- Write down examples
(test pi-provider 3.1415926535897)

; Taxation:
;   1- Determine how the information involved is encoded as data
;       - gross-pay : Number
;       - 
;   2- Describe what your program should do in prose, giving it a name
;       Taxation without Representation finds the tax rate for a 
;           given gross-pay. 
;   3- Write down the contract of your program
;       - What it assumes about inputs
;       - What it guarantees about outputs
(define-type-alias gross-pay Number)
(tax : (Number -> Number))
;   5- Write down the template
;   6- Fill up that template
(define (tax gross-pay)
    (let 
    (
        [amount-over-480 (- gross-pay 480)]
        [amount-over-240 (- gross-pay 240)]
    )
        (cond
            [(> amount-over-480 0.0) (+ (* amount-over-480 0.28) 36)]
            [(> amount-over-240 0.0) (* amount-over-240 0.15)]
            [else 0.0]
        )
    )
    ;;; (cond
    ;;;     [(>= gross-pay 480.1) (* gross-pay 0.28)]
    ;;;     [(>= gross-pay 240.1) (* gross-pay 0.15)]
    ;;;     [(<= gross-pay 240.0) 0.0])
)

;   4- Write down examples
(test (tax 240.0) 0.0)
(test (tax 250.0) 1.5)
(test (tax 480.0) 36.0)
(test (tax 580.0) 64.0)

; Net Pay:
;   1- Determine how the information involved is encoded as data
;       - hours-worked : Number 
;       - hourly-wage : Number
;   2- Describe what your program should do in prose, giving it a name
;       - PayDay calculates what is the net-pay of a given employee, 
;           given his/her number of hours worked and hourly wage
;   3- Write down the contract of your program
;       - What it assumes about inputs
;       - What it guarantees about outputs
(define-type-alias hours-worked Number)
(define-type-alias hourly-wage Number)
(net-pay : (Number Number -> Number))
;   5- Write down the template
;   6- Fill up that template
(define (net-pay hours-worked hourly-wage)
    (let ([gross-pay (* hours-worked hourly-wage)])
        (- gross-pay (tax gross-pay))
    )
)
;   4- Write down examples
(test (net-pay 20 40) 674.4)
(test (net-pay 40 16) 559.2)
(test (net-pay 40 15.5) 544.8)
