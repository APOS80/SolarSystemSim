#lang racket

(require racket/gui/base)

; The gravitational constant G
(define G 6.67428e-11)

; Assumed scale: 100 pixels = 1AU
(define AU (* 149.6e6 1000))
(define SCALE (/ 250 AU))

(struct body (id px py vx vy mass radius color) #:mutable #:transparent) ;Structure of body
;position in m, vector in m/s, mass in kg, radius in m

(define (force g mass otherMass distance) ;Calculate the force of attraction
(/ (* g (* mass otherMass)) (expt distance 2)))

(define (directionOfForce dx dy force) ;Calculate direction of the force
(let ([theta (atan dy dx)])
(list (* (cos theta) force) (* (sin theta) force))))

(define (attraction body otherBody) ;Creates a vector to adjust planet heading depending on all other bodies 
(let* ([dx (- (body-px otherBody) (body-px body))]
[dy (- (body-py otherBody) (body-py body))]
[distance (sqrt (+ (expt dx 2) (expt dy 2)))]) ;Distance between bodys
(if (= distance 0) (print "Hitt!")
(directionOfForce dx dy
(force G (body-mass body) (body-mass otherBody) distance)))))

(define timestep (* 12 3600)) ;Half a day

(define (totalAttraction body bodies fxy) ;Creates a list of vectors, a vector for every body
(if (equal? bodies '())
fxy
(totalAttraction body (cdr bodies) (map + fxy (attraction body (car bodies)))))
)

(define (gravity bodies timestep)
(let* ([forces (for/list ([b bodies]) (totalAttraction b (remove b bodies) '(0 0)))]
[vectors (for/list ([f forces][b bodies]) (list (+ (body-vx b) (*(/ (car f) (body-mass b)) timestep))
(+ (body-vy b) (* (/(car(cdr f)) (body-mass b)) timestep))))]
[positions (for/list ([v vectors][b bodies]) (list (+ (body-px b) (* (car v) timestep))
(+ (body-py b) (* (car (cdr v)) timestep))))])

(for/list ([b bodies][v vectors][p positions])
(body (body-id b) (car p) (car(cdr p)) (car v) (car(cdr v))
(body-mass b) (body-radius b) (body-color b)))
))
;(struct body (id px py vx vy mass radius color)) ;just a reminder of the struct


;A list of bodies, size of planets is not real... you woldent se the planets. 
(define testCollPlanets (list
(body "Sun" 0 0 0 0 (* 1.98892 (expt 10 30)) 100 "yellow")
(body "Mercury" (* -0.387098 AU) 0 0 (* -47.362 1000) (* 3.3011 (expt 10 23)) 4 "red")
(body "Venus" (* 0.723 AU) 0 0 (* 35.02 1000) (* 4.8685 (expt 10 24)) 8 "brown")
(body "Earth" (* -1 AU) 0 0 (* -29.783 1000) (* 5.9742 (expt 10 24)) 8 "green")
(body "Mars" (* -1.5236 AU) 0 0 (* -24.077 1000) (* 6.4174 (expt 10 23)) 4 "orange")
;(body "Havoc" (* -1.2 AU) 0 0 (* -10 1000) (* 8 (expt 10 25)) 50 "green")
))



(define (printBodies bodies scale) ;To print the numbers for control
(if (equal? bodies '())
(printf "Done\n")
(let
([ p (printf "Position XY ~a \n" (list (body-id (car bodies))
(* (body-px (car bodies)) scale)
(* (body-py (car bodies)) scale)
(* (body-vx (car bodies)) scale)
(* (body-vy (car bodies)) scale)))])
(printBodies (cdr bodies) scale))))

(define (loop grav bodies timestep scale n);A numeric simulation
(printBodies bodies scale)
(if (> n 0)
(loop grav (gravity bodies timestep) timestep scale (- n 1))
(printf "End")
))


;(loop G testCollPlanets timestep SCALE 90)

;A gui below
(define myframe (new frame%
[width 100]
[height 100]
[label "Might be a CAD someday."]))

(define (solarPainter grav bodies timestep scale);Update planet positions and paint
(let ([ bp (gravity bodies timestep)])
(for ([b bp][i (length bodies)])
;mutate struct
(set-body-px! (list-ref testCollPlanets i) (body-px b))
(set-body-py! (list-ref testCollPlanets i) (body-py b))
(set-body-vx! (list-ref testCollPlanets i) (body-vx b))
(set-body-vy! (list-ref testCollPlanets i) (body-vy b))
;paint
(send dc set-brush (make-object brush% (body-color b) 'solid))
(send dc draw-ellipse
(+ (* (body-px b) scale) (- 500 (/(body-radius b) 2)))
(+ (* (body-py b) scale) (- 500 (/(body-radius b) 2)))
(body-radius b)
(body-radius b))
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
(solarPainter G testCollPlanets timestep SCALE))] ;Call the planetpainter
))


(define refreshTimer ;Canvas refresh
(new timer% [notify-callback (lambda () (send my_canvas refresh))]))

(define dc (send my_canvas get-dc))

(send myframe show #t)
(send refreshTimer start 16 #f) ;Start refresh timer
