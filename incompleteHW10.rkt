;; 2htdp/image: Library
;;Allows the program to handle the image of the duck
(require 2htdp/image)

;;2htdp/universe: Library
;;track times and state of the duck/components
(require 2htdp/universe)


;;Constants
(define START-X 200)
(define START-Y 400)
(define MAX-DIST 80)
(define ACCELERATION 200)

(define WIDTH 800)
(define HEIGHT 600)
(define DUCK  .)
;; May also be refered to as "Bob"

;;Part 1
;;slingshot-position-from-angle : num num -> posn
;; Takes in a radian and a distance and outputs a new posn
(define (slingshot-position-from-angle angle distance)
  (make-posn
   (+ START-X (* (min distance MAX-DIST) (cos angle))) (+ START-Y (* (min distance MAX-DIST) (sin angle)))))

(check-expect (slingshot-position-from-angle 0 50) (make-posn 250 400))
(check-within (slingshot-position-from-angle (/ pi 2) 80) (make-posn 200 480) 0.01)

;;Part 2
;; slingshot-position : num num -> posn
;; Takes in the x and y coordinate of the mouse and returns a posn of the duck's location
(define (slingshot-position mouse-x mouse-y)
  (slingshot-position-from-angle
   (atan (- mouse-y START-Y) (- mouse-x START-X))
   (sqrt (+ (sqr (- mouse-x START-X)) (sqr(- mouse-y START-Y))))))

(check-within (slingshot-position (- START-X 100) (- START-Y 100)) (make-posn (- START-X (/ MAX-DIST (sqrt 2))) (- START-Y (/ MAX-DIST (sqrt 2)))) 0.01)
(check-within (slingshot-position 210 420)(make-posn 210 420) 0.01)

;;Part 3
;; duck-at-posn-scene : posn -> image
;; Takes in a position and outputs the postion of the duck on a blank screen
(define (duck-at-posn-scene duck-postion)
  (place-image DUCK
               (posn-x duck-postion)
               (posn-y duck-postion)
               (empty-scene WIDTH HEIGHT)))

;;No check-expect needed for part 3 :D



;; A duck is:
;;     (make-duck posn num num)
;; dx and dy are velocities in the x & y-directions respectfully
;; Velocity is measured in pixels/second
(define-struct duck (loc dx dy))


;;Part 4
;; fling-duck : posn -> duck
;; Takes in the curent position of the duck and outputs the velocity using
;; The change in x and y to calculate the 2 numbers for velocity
(define (fling-duck curr-pos)
  (make-duck curr-pos
             (* 4 (- START-X (posn-x curr-pos)))
             (* 4 (- START-Y (posn-y curr-pos)))))

(check-expect (fling-duck (make-posn (- START-X 10) (+ START-Y 30))) (make-duck (make-posn (- START-X 10) (+ START-Y 30)) 40 -120))
(check-expect (fling-duck (make-posn 220 350)) (make-duck (make-posn 220 350) -80 200))



;;Part 5
;; step-duck : num duck -> duck
;; Takes in a fraction of a second and a duck and outputs
;; a moving duck and affected by the for of gravity
(define (step-duck time current-duck)
  (make-duck
   (make-posn
    (+ (posn-x (duck-loc current-duck)) (* time (duck-dx current-duck)))
    (+ (posn-y (duck-loc current-duck)) (* time (duck-dy current-duck))))
   (duck-dx current-duck)
   (+ (duck-dy current-duck) (* time ACCELERATION))))

(check-expect (step-duck 1/2 (make-duck (make-posn 100 200) 10 -100))(make-duck (make-posn 105 150) 10 0))
(check-expect (step-duck 1 (make-duck (make-posn 100 200) 10 50)) (make-duck (make-posn 110 250) 10 250))



;;Part 6
;; A Launch is one of:
;; - False
;; - Posn
;; - Duck
;; False means the duck is on standby
;; Posn means the duck is being dragged around
;; Duck means that the duck is in the air
;; To render the duck onto the scene with the appropiate state
(define (launch-scene launch)
  (cond
    [(false? launch) (duck-at-posn-scene (make-posn START-X START-Y))]
    [(posn? launch) (duck-at-posn-scene launch)]
    [(duck? launch) (duck-at-posn-scene (duck-loc launch))]))

(check-expect (launch-scene (make-duck (make-posn 100 200) 5 5))(duck-at-posn-scene (make-posn 100 200)))
(check-expect (launch-scene false) (duck-at-posn-scene (make-posn START-X START-Y)))
(check-expect (launch-scene (make-posn 67 67)) (duck-at-posn-scene (make-posn 67 67)))



;;Part 7
;; step-launch : launch -> launch
;; Takes in a launch variable and outputs the state of the duck
;; Returns false if the duck:
;; - Hasn't been touched
;; - Is out of bounds
;; Else, it will return the moving and accelerating duck
(define (step-launch lunch)
  (cond
    [(false? lunch) #false]
    [(posn? lunch) lunch]
    [(and (duck? lunch)(or
                        (>= (posn-x (duck-loc lunch)) WIDTH)
                        (< (posn-x (duck-loc lunch)) 0)
                        (>= (posn-y (duck-loc lunch)) HEIGHT)
                        (< (posn-y (duck-loc lunch)) 0))
          ) #false]
    [else (step-duck 1/28 lunch)]))

(check-expect (step-launch #false) #false)
(check-expect (step-launch (make-posn 100 100)) (make-posn 100 100))
(check-expect (step-launch (make-duck (make-posn WIDTH HEIGHT) 1 1)) #false)
(check-expect (step-launch (make-duck (make-posn 0 0) 10 10)) (step-duck 1/28 (make-duck (make-posn 0 0) 10 10)))

;; I am aware of the 0 0 thing, this was intentional lol

;; Part 8
;; mouse-launch : launch num num string -> launch
;; To have the state of launching Bob be reflected on clicks and release
;; Takes in launch, x y position of mouse, and current state to produce a launch accordingly
(define (mouse-launch dinner mouse-x mouse-y state)
  (cond
    [(false? dinner)
     (if (string=? state "button-down") (slingshot-position mouse-x mouse-y) #false)]
   
    [(posn? dinner)
     (cond
       [(string=? state "button-up") (fling-duck (slingshot-position mouse-x mouse-y))]
       [(string=? state "drag") (slingshot-position mouse-x mouse-y)]
       [else dinner])]

    [(duck? dinner) dinner]))

(check-within (mouse-launch #false 100 100 "button-down") (slingshot-position 100 100) 0.01)
(check-within (mouse-launch (make-posn 100 100) 110 110 "drag")(slingshot-position 110 110)  0.01)
(check-expect (mouse-launch (make-duck (make-posn 200 200) 10 10) 50 50 "button-down")(make-duck (make-posn 200 200) 10 10))
(check-expect (mouse-launch (make-duck (make-posn 37 37) 20 20) 10 10 "my-name-is-bob!") (make-duck (make-posn 37 37) 20 20))


(check-expect (mouse-launch (make-posn 67 67) 0 0 "please-claude-我需要这个") (make-posn 67 67))


(check-within (mouse-launch (make-posn 10 10) 0 0 "button-up") (fling-duck (slingshot-position 0 0)) 0.01)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A block is a (make-block number number string)
;; The string is a color already implemented into racket, ie. "tomato"
(define-struct block (x y color))

;;;Part 1
;; gen-blocks : natural num num string -> list of blocks
;; Creates a list of n blocks, repersented by the natural number,
;; starting at the 2 coordinates in a certain color
(define (gen-blocks n x0 y0 col)
  (cond
    [(= n 0) empty]
    [else
     (cons (make-block x0 y0 col) (gen-blocks (sub1 n) x0 (+ 30 y0) col))]))

(check-expect (gen-blocks 2 100 100 "red")
              (list (make-block 100 100 "red")
                    (make-block 100 130 "red")))

(check-expect (gen-blocks 0 100 100 "tomato")
              empty)

;;; Helper Function for part 2
;; touches-target? block target -> bool
;; Checks and determines whether a given block is touch the target,
;; Deciding whether the block is within the 30 unit range in the y and x direction
(define (touch-target? block target)
  (cond
    [(and
      (and (<= (block-x block)(posn-x target)) (>= (+ 30 (block-x block)) (posn-x target)))
      (and (<= (block-y block)(posn-y target)) (>= (+ 30 (block-y block)) (posn-y target))))
     #true]
    [else #false]))

(check-expect (touch-target? (make-block 100 100 "red") (make-posn 1 1)) #false)
(check-expect (touch-target? (make-block 30 30 "tomato")(make-posn 35 35)) #true)
(check-expect (touch-target? (make-block 30 30 "tomato")(make-posn 25 35)) #false)

;;; Part 2
;; remove-blocks : list-of-blocks posn -> list-of-blocks
;; Removes all the blocks that touches the target
(define (remove-blocks blocks target)
  (cond
    [(empty? blocks) empty]
    [(touch-target? (car blocks) target) (remove-blocks (cdr blocks) target)]
    [else (cons (car blocks) (remove-blocks (cdr blocks) target))]))

(check-expect
 (remove-blocks (gen-blocks 2 10 10 "blue") (make-posn 20 20))
 (gen-blocks 1 10 40 "blue"))

(check-expect
 (remove-blocks (gen-blocks 3 67 67 "lime") (make-posn 1 1))
 (list (make-block 67 67 "lime") (make-block 67 97 "lime") (make-block 67 127 "lime")))

;;;Part 3
;; add-blocks-to-scene : list-of-blocks Scene -> Scene
;; Takes the list of blocks and now actually draws them in racket
(define (add-blocks-to-scene blocks scene)
  (cond
    [(empty? blocks) scene]
    [else
     (place-image
      (square 30 "solid" (block-color (car blocks)))
      (block-x (car blocks))
      (block-y (car blocks))
      (add-blocks-to-scene (cdr blocks) scene))]))

(check-expect (add-blocks-to-scene (gen-blocks 1 50 50 "orange") (empty-scene WIDTH HEIGHT))
              (place-image (square 30 "solid" "orange") 50 50 (empty-scene WIDTH HEIGHT)))

;;;Part 4

;; A game is
;;   (make-game launch list-of-block)
(define-struct game (launch blocks))

;; game-scene : game -> scene
;; Takes in a game, with a state of launch and list of blocks
;; This will produce a scene with the duck
;; The blocks
(define (game-scene mash)
  (add-blocks-to-scene (game-blocks mash)
                       (launch-scene (game-launch mash))))
(check-expect
 (game-scene (make-game #false (gen-blocks 0 0 0 "yellow")))
 (place-image DUCK START-X START-Y (empty-scene WIDTH HEIGHT)))
(check-expect
 (game-scene (make-game (make-posn 0 0) (gen-blocks 0 0 0 "green")))
 (place-image DUCK 0 0 (empty-scene WIDTH HEIGHT)))
 

;; mouse-game : game num num string -> game
;; The 2 nums repersent the x and y positions of the duck
;; The string repersents the state of mouses, tracking clicks, etc
(define (mouse-game g x y state)
  (make-game
   (mouse-launch (game-launch g) x y state)
   (game-blocks g)))

(check-expect
 (mouse-game (make-game #false (gen-blocks 2 10 10 "blue")) 0 0 "drag")
 (make-game #false (list (make-block 10 10 "blue") (make-block 10 40 "blue"))))
(check-expect
 (mouse-game (make-game (make-duck (make-posn 1 1) 0 0) (gen-blocks 1 10 10 "blue")) 0 0 "button-down")
 (make-game (make-duck (make-posn 1 1) 0 0) (list (make-block 10 10 "blue"))))


;; step-game : game -> game
;; Takes in a game and returns a new game
;; Changing the launch inside of game
(define (step-game g)
  (combine-game (step-launch (game-launch g)) (game-blocks g)))

(check-expect
 (step-game (make-game #false (gen-blocks 2 100 100 "lime")))
 (make-game #false (list (make-block 100 100 "lime") (make-block 100 130 "lime"))))
(check-expect
 (step-game (make-game (make-posn 1 1) (gen-blocks 2 50 50 "orange")))
 (make-game (make-posn 1 1) (list (make-block 50 50 "orange") (make-block 50 80 "orange"))))

   
;;;Part 5
;; Helper Function
;; combine-game : launch list-of-blocks -> game
;; Takes the state of the launch, and the list of blocks
;; It returns a game based on the state
(define (combine-game launch blocks)
  (cond
    [(duck? launch) (make-game launch
                               (remove-blocks blocks (duck-loc launch)))]
    [else (make-game launch blocks)]))

(check-expect (combine-game #false (gen-blocks 2 10 10 "blue"))
              (make-game #false (list (make-block 10 10 "blue") (make-block 10 40 "blue"))))
(check-expect (combine-game (make-duck (make-posn 67 67) 6 7)(gen-blocks 1 67 67 "purple"))
              (make-game (make-duck (make-posn 67 67) 6 7) empty))









(big-bang
  (make-game false (gen-blocks 10 600 300 "royalblue"))
  [to-draw game-scene]
  [on-tick step-game]
  [on-mouse mouse-game])
