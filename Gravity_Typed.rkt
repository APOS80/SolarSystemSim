#lang typed/racket


(require racket/flonum)
(require "mvmath_typed.rkt")

(provide (all-defined-out))

; The gravitational constant G
(define G 6.67428e-11)

; Assumed scale: 100 pixels = 1AU
(define AU 149597870700.0) ;Astronomic Unit (m)
(define SCALE (/ 250.0 AU))
(define timestep (* 12.0 3600.0)) ;Half a day

;(struct pt ([x : Float][y : Float]))

(struct body ([id : String] [px : Float] [py : Float] [vx : Float]
              [vy : Float] [mass : Float] [radius : Float] [color : String]) #:mutable #:transparent) ;Structure of body
;position in m, vector in m/s, mass in kg, radius

;future struct thoughts
;(struct body (id px py vx vy mass radius color friction elasticy sord) #:mutable #:transparent) ;sord->static or dynamic
;(: distance (-> pt pt Real))

(: force : (-> Float Float Float Float Float))
(define (force g mass otherMass distance) ;Calculate the force of attraction
  (fl/ (fl* g (fl* mass otherMass)) (flexpt distance 2.0)))

(: directionOfForce : (-> Float Float Float (Listof Float)))
(define (directionOfForce dx dy force) ;Calculate direction of the force
  (let ([theta (atan dy dx)])
    (list (fl* (flcos theta) force) (fl* (flsin theta) force))))

(: bounce : (-> (Listof Float) (Listof Float) (Listof Float)))
(define (bounce v n) ;v velosity vector, n normal vector, wall bounce type
  (let* ([u (vectorScalarMult (fl/(vectorDotProduct v n)
                                (vectorDotProduct n n)) n)] ;Frictionless
         [w (vectorSub v u)]
         ;[pr (printf "v ~a u ~a w ~a\n" v u w)]
         [f 0.8];friction
         [r 0.8];elasticity
         [vn (vectorSub (vectorScalarMult f w)(vectorScalarMult r u))]
         ;[pr2 (printf "vn ~a\n" vn)]
         )
    vn))

;(bounce2D '(-1 0) '(1 0) 100 100)

(: bounceE : (-> (Listof Float) (Listof Float) (Listof Float) Float Float (Listof Float)))
(define (bounceE v1 v2 n m1 m2) ;v velosity vector, n normal vector, bounce with momentum transfer
  (let* (;[prr2 (printf "v1: ~a v2: ~a\n" v1 v2)]
         [p1 (vectorScalarMult m1 v1)]
         [p2 (vectorScalarMult m2 v2)]
         [ps (vectorAdd p1 p2)]
         [v1b (vectorScalarMult (vectorMagn (vectorScalarDiv p2 m1)) (vectorNorm v1))]
         [u (vectorScalarMult (/(vectorDotProduct v1b n)
                                (vectorDotProduct n n)) n)] ;Frictionless
         [w (vectorSub v1b u)]
         [f 0.8];friction
         [r 0.8];elasticity
         [vn (vectorSub (vectorScalarMult f w)(vectorScalarMult r u))]
         )
    vn))

(: bounceS : (-> (Listof Float) (Listof Float) (Listof Float)))
(define (bounceS v1 n) ;Simplest reflection, the core, frictionless wall bounce
  (let* ([u (vectorScalarMult (fl* 2.0 (vectorDotProduct v1 n)) n)]
         [w (vectorSub v1 u)]
         ;[pr (printf "v ~a u ~a w ~a\n" v1 u w)]
         )
    w))

(: attraction : (-> body body (Listof Float)))
(define (attraction body otherBody) ;Creates a vector to adjust planet heading depending on all other bodies 
  (let* ([dx (fl- (body-px otherBody) (body-px body))]
         [dy (fl- (body-py otherBody) (body-py body))]
         [distance (flsqrt (fl+ (flexpt dx 2.0) (flexpt dy 2.0)))]) ;Distance between bodys
         (directionOfForce dx dy
                           (force G (body-mass body) (body-mass otherBody) distance))))

(: collisionDetection : (-> body body (Listof Float)))
(define (collisionDetection body otherBody)
  (let* ([dx (fl- (body-px otherBody) (body-px body))]
         [dy (fl- (body-py otherBody) (body-py body))]
         [distance (flsqrt (fl+ (flexpt dx 2.0) (flexpt dy 2.0)))]
         [rsum (fl+ (body-radius body) (body-radius otherBody))])
  (if (fl>  rsum distance)
                 (let* ([b (bounceE (list(body-vx body)(body-vy body))
                                     (list(body-vx otherBody)(body-vy otherBody))
                                     (vectorNorm (list dx dy))
                                     (body-mass body)
                                     (body-mass otherBody))]
                        [resetV (vectorScalarMult  (fl+ (fl* (fl- rsum distance)
                                                        (fl/ (body-mass otherBody)
                                                        (fl+ (body-mass body)(body-mass otherBody))))distance)
                                                   (vectorNorm(list (fl* dx -1.0) (fl* dy -1.0))))]
                        [resetTo (list (fl+ (list-ref resetV 0) (body-px otherBody))
                                       (fl+ (list-ref resetV 1) (body-py otherBody)))]
                        )(append resetTo b))
                 '())))

(: totalAttraction : (-> body (Listof body) (Listof Float) (Listof Float)))
(define (totalAttraction body bodies fxy) ;Creates a list of vectors, a vector for every body
  (if (equal? bodies '())
      fxy
      (totalAttraction body (cdr bodies) (map fl+ fxy (attraction body (car bodies)))))
  )

(: totalCollision : (-> body (Listof body) (Listof Float) (Listof Float)))
(define (totalCollision body bodies col) ;Creates a list of vectors, a vector for every body
  (if (equal? bodies '())
      (let ([cola col])
        (if (equal? '() cola) (list (body-px body)(body-py body)(body-vx body)(body-vy body)) cola))
      (totalCollision body (cdr bodies) (append col (collisionDetection body (car bodies)))))
  )

(: gravity : (-> (Listof body) Float (Listof body)))
(define (gravity bodies timestep)
  (let* ([bodiesCd (for/list : (Listof (Listof Float)) ([b : body bodies]) (totalCollision b (remove b bodies) '()))]
         [forces (for/list : (Listof (Listof Float)) ([b : body bodies]) (totalAttraction b (remove b bodies) '(0.0 0.0)))]
         [vectors (for/list : (Listof (Listof Float)) ([f : (Listof Float) forces][b : body bodies][cd bodiesCd]) (list (fl+ (list-ref cd 2) (fl* (fl/ (car f) (body-mass b)) timestep))
                                                                      (fl+ (list-ref cd 3) (fl* (fl/(car(cdr f)) (body-mass b)) timestep))))]
         [positions (for/list : (Listof (Listof Float)) ([v vectors][cd bodiesCd]) (list (fl+ (list-ref cd 0) (fl* (car v) timestep))
                                                               (fl+ (list-ref cd 1) (fl* (car (cdr v)) timestep))))])
    (for/list ([b bodies][v vectors][p positions])
      (body (body-id b)
            (car p) ;px
            (car(cdr p)) ;py
            (car v) ;vx
            (car(cdr v)) ;vy
            (body-mass b)
            (body-radius b)
            (body-color b)))
    ))



