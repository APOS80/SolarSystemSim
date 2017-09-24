#lang racket

(require racket/flonum)
(require racket/gui/base)
(require "mvmath.rkt")

; The gravitational constant G
(define G 6.67428e-11)

; Assumed scale: 100 pixels = 1AU
(define AU 149597870700.0) ;Astronomic Unit (m)
(define SCALE (/ 250.0 AU))
(define timestep (* 12.0 3600.0)) ;Half a day

(struct body (id px py vx vy mass radius color) #:mutable #:transparent) ;Structure of body
;position in m, vector in m/s, mass in kg, radius

;future struct thoughts
;(struct body (id px py vx vy mass radius color friction elasticy sord) #:mutable #:transparent) ;sord->static or dynamic

(define (force g mass otherMass distance) ;Calculate the force of attraction
  (fl/ (fl* g (fl* mass otherMass)) (flexpt distance 2.0)))

(define (directionOfForce dx dy force) ;Calculate direction of the force
  (let ([theta (atan dy dx)])
    (list (fl* (flcos theta) force) (fl* (flsin theta) force))))

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

(define (bounceS v1 n) ;Simplest reflection, the core, frictionless wall bounce
  (let* ([u (vectorScalarMult (fl* 2.0 (vectorDotProduct v1 n)) n)]
         [w (vectorSub v1 u)]
         ;[pr (printf "v ~a u ~a w ~a\n" v1 u w)]
         )
    w))

(define (attraction body otherBody) ;Creates a vector to adjust planet heading depending on all other bodies 
  (let* ([dx (fl- (body-px otherBody) (body-px body))]
         [dy (fl- (body-py otherBody) (body-py body))]
         [distance (flsqrt (fl+ (flexpt dx 2.0) (flexpt dy 2.0)))]) ;Distance between bodys
         (directionOfForce dx dy
                           (force G (body-mass body) (body-mass otherBody) distance))))

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
                 '("n"))))

(define (totalAttraction body bodies fxy) ;Creates a list of vectors, a vector for every body
  (if (equal? bodies '())
      fxy
      (totalAttraction body (cdr bodies) (map fl+ fxy (attraction body (car bodies)))))
  )

(define (totalCollision body bodies col) ;Creates a list of vectors, a vector for every body
  (if (equal? bodies '())
      (let* ([cola (remove* (list "n") col equal?)]
            [p (printf "cola ~a\n" cola)]
            )
        (if (equal? '() cola) (list (body-px body)(body-py body)(body-vx body)(body-vy body)) cola))
      (totalCollision body (cdr bodies) (append col (collisionDetection body (car bodies)))))
  )

(define (gravity bodies timestep)
  (let* ([bodiesCd (for/list ([b bodies]) (totalCollision b (remove b bodies) '()))]
         [forces (for/list ([b bodies]) (totalAttraction b (remove b bodies) '(0.0 0.0)))]
         [vectors (for/list ([f forces][b bodies][cd bodiesCd]) (list (fl+ (list-ref cd 2) (fl* (fl/ (car f) (body-mass b)) timestep))
                                                                      (fl+ (list-ref cd 3) (fl* (fl/(car(cdr f)) (body-mass b)) timestep))))]
         [positions (for/list ([v vectors][cd bodiesCd]) (list (fl+ (list-ref cd 0) (fl* (car v) timestep))
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

;A list of bodies, size of planets is not real... you woldent se the planets. 
(define testCollPlanets (list
                         (body "Sun" 0.0 0.0 0.0 0.0 (* 1.98892 (expt 10 30)) (* 0.15 AU) "yellow")
                         (body "Mercury" (* -0.387098 AU) 0.0 0.0 (* -47.362 1000) (* 3.3011 (expt 10 23)) (* 0.01 AU) "red")
                         (body "Venus" (* 0.723 AU) 0.0 0.0 (* 35.02 1000) (* 4.8685 (expt 10 24)) (* 0.02 AU) "brown")
                         (body "Earth" (* -1.0 AU) 0.0 0.0 (* -29.783 1000) (* 5.9742 (expt 10 24)) (* 0.02 AU) "green")
                         (body "Mars" (* -1.5236 AU) 0.0 0.0 (* -24.077 1000) (* 6.4174 (expt 10 23)) (* 0.015 AU) "orange")
                         (body "Havoc" (* -0.5 AU) 0.0 (* 5.0 1000) (* -0.5 1000) (* 10.0 (expt 10 24)) (* 0.05 AU) "green")
                         (body "Midget" (* 0.5 AU) 0.0  (* -7.0 1000) (* 0.5 1000) (* 10.0 (expt 10 24)) (* 0.05 AU) "blue")
                         ))

;A gui below
(define myframe (new frame%
                     [width 100]
                     [height 100]
                     [label "Solar system simulator"]))

(define (solarPainter bodies timestep scale);Update planet positions and paint
    (let ([ bp (gravity bodies timestep)])
      (set! testCollPlanets bp);>>>Dangerus mutation!!<<<
      (for ([b bp])
        ;paint
        (send dc set-brush (make-object brush% (body-color b) 'solid))
        (send dc draw-ellipse
              (+ (* (- (body-px b) (body-radius b)) scale) 500.0)
              (+ (* (- (body-py b) (body-radius b)) scale) 500.0)
              (* (* (body-radius b) 2.0) scale)
              (* (* (body-radius b) 2.0) scale))
        )))
  
(define my_canvas (new canvas% ;Only a canvas
                 [parent myframe]
                  [min-width 1000]
                  [min-height 1000]
                  [paint-callback 
                     (lambda(canvas dc)
                       (send dc set-smoothing 'smoothed)
                       (send dc erase)
                       (send dc set-brush (make-object brush% "black" 'solid))
                       (send dc draw-rectangle 0 0 1000 1000)
                       (send dc set-alpha 1)
                       (solarPainter testCollPlanets timestep SCALE))] ;Call the planetpainter
                  ))


(define refreshTimer ;Canvas refresh
  (new timer% [notify-callback (lambda () (send my_canvas refresh))]))

(define dc (send my_canvas get-dc))

(send myframe show #t)
(send refreshTimer start 16 #f) ;Start refresh timer

