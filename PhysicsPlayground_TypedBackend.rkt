#lang racket

(require racket/gui/base)
(require "Gravity_typed.rkt")

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
