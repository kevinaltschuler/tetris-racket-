;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions


;; A Block is a (make-block Number Number Color)
(define-struct block (x y color))

;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates
;; when it spins.
(define-struct tetra (center blocks))

;; A [Listof X] is one of:
;; - empty
;; - (cons X [Listof X])

;; A Set of Blocks (BSet) is [Listof Block]
;; Order does not matter.  Repetitions are NOT allowed.

;; A World is a (make-world Tetra BSet)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))

#; (define (block-template a-block)
     ... (block-x a-block) ...
     ... (block-y a-block) ...
     ... (block-color a-block) ...)

#; (define (tetra-template a-tetra)
     ... (tetra-center a-tetra) ...
     ... (bset-template (tetra-blocks a-tetra)) ...)

#; (define (bset-template a-bset)
     (cond [(empty? a-bset) ...]
           [else
            ... (block-template (first a-bset)) ...
            ... (bset-template (rest a-bset)) ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
(define TICKRATE 1/3)
(define BOARDWIDTH 10)
(define BOARDHEIGHT 20)
(define PIXELS/CELL 25)
(define BG (empty-scene (* PIXELS/CELL BOARDWIDTH)
                        (* PIXELS/CELL BOARDHEIGHT)))

(define INITIAL-Y-1 0)
(define INITIAL-Y-2 1)
(define INITIAL-X   (floor (/ BOARDWIDTH 2)))
(define INITIAL-X+1 (+ INITIAL-X 1))
(define INITIAL-X+2 (+ INITIAL-X 2))
(define INITIAL-X-1 (- INITIAL-X 1))
(define INITIAL-X-2 (- INITIAL-X 2))




(define tetra-o (make-tetra (make-posn (+ INITIAL-X-1 .5) (+ INITIAL-Y-1 .5)) 
                            (list (make-block INITIAL-X-1 INITIAL-Y-1 'green) 
                                  (make-block INITIAL-X INITIAL-Y-1 'green)
                                  (make-block INITIAL-X-1 INITIAL-Y-2 'green) 
                                  (make-block INITIAL-X INITIAL-Y-2 'green))))

(define tetra-i (make-tetra (make-posn (+ INITIAL-X-1 .5) (+ INITIAL-Y-1 .5))
                            (list (make-block INITIAL-X-2 INITIAL-Y-1 'blue) 
                                  (make-block INITIAL-X-1 INITIAL-Y-1 'blue)
                                  (make-block INITIAL-X INITIAL-Y-1 'blue) 
                                  (make-block INITIAL-X+1 INITIAL-Y-1 'blue))))

(define tetra-l (make-tetra (make-posn INITIAL-X INITIAL-Y-1)
                            (list (make-block INITIAL-X-1 INITIAL-Y-1 'purple) 
                                  (make-block INITIAL-X INITIAL-Y-1 'purple)
                                  (make-block INITIAL-X+1 INITIAL-Y-1 'purple) 
                                  (make-block INITIAL-X+1 INITIAL-Y-2 'purple))))

(define tetra-j (make-tetra (make-posn INITIAL-X-1 INITIAL-Y-1)
                            (list (make-block INITIAL-X-1 INITIAL-Y-1 'cyan) 
                                  (make-block INITIAL-X-1 INITIAL-Y-2 'cyan)
                                  (make-block INITIAL-X INITIAL-Y-1 'cyan) 
                                  (make-block INITIAL-X+1 INITIAL-Y-1 'cyan))))

(define tetra-t (make-tetra (make-posn INITIAL-X INITIAL-Y-1)
                            (list (make-block INITIAL-X-1 INITIAL-Y-1 'orange) 
                                  (make-block INITIAL-X INITIAL-Y-1 'orange)
                                  (make-block INITIAL-X INITIAL-Y-2 'orange) 
                                  (make-block INITIAL-X+1 INITIAL-Y-1 'orange))))

(define tetra-z (make-tetra (make-posn INITIAL-X INITIAL-Y-1)
                            (list (make-block INITIAL-X-1 INITIAL-Y-2 'pink) 
                                  (make-block INITIAL-X INITIAL-Y-1 'pink)
                                  (make-block INITIAL-X INITIAL-Y-2 'pink) 
                                  (make-block INITIAL-X+1 INITIAL-Y-1 'pink))))

(define tetra-s (make-tetra (make-posn INITIAL-X INITIAL-Y-1)
                            (list (make-block INITIAL-X-1 INITIAL-Y-1 'red) 
                                  (make-block INITIAL-X INITIAL-Y-1 'red)
                                  (make-block INITIAL-X INITIAL-Y-2 'red) 
                                  (make-block INITIAL-X+1 INITIAL-Y-2 'red))))


(define INITIAL-WORLD (make-world tetra-j empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS


;;;;;;;;;;;;;;;;;RENDERING;;;;;;;;;;;;;;;

;; draw-world places both bsets onto the bg
;; world -> image 
(define (draw-world world)
  (place-bset (append (world-pile world) 
                      (tetra-blocks (world-tetra world)))
              BG))

;; block -> image
;; block->image turns a block struct into a (square xy color)
(define (block->image block)
  (square PIXELS/CELL "solid" (block-color block)))

;; place-bset places a bset onto a background
;; bset -> image
(define (place-bset bset image)
  (cond [(empty? bset) image]
        [else (place-image
            (block->image (first bset))
            (* PIXELS/CELL (- (block-x (first bset)) .5)) (* PIXELS/CELL (- (block-y (first bset)) .5))
            (place-bset (rest bset) BG))]))

(check-expect (place-bset (tetra-blocks tetra-o) BG) (place-image  (square PIXELS/CELL "solid" "green") 
                                                    (* PIXELS/CELL (- INITIAL-X-1 .5)) (* PIXELS/CELL (- INITIAL-Y-1 .5))
                                                    (place-image (square PIXELS/CELL "solid" "green")
                                                                  (* PIXELS/CELL(- INITIAL-X-1 .5))  (* PIXELS/CELL (- INITIAL-Y-2 .5))
                                                                 (place-image (square PIXELS/CELL "solid" "green") 
                                                                               (* PIXELS/CELL (- INITIAL-X .5)) (* PIXELS/CELL (- INITIAL-Y-1 .5))
                                                                               (place-image (square PIXELS/CELL "solid" "green")
                                                                                             (* PIXELS/CELL (- INITIAL-X .5))  (* PIXELS/CELL (- INITIAL-Y-2 .5))
                                                                                            BG)))))
;;;;;;;;;;;;;;;MOVEMENT;;;;;;;;;;;;;;;;;;;

;; key takes an input and interprets
;; world - key -> world
(define (key world key)
  (make-world  (cond [(key=? "left" key) (make-tetra (posn-move (tetra-center (world-tetra world)) "left") (bset-move (tetra-blocks (world-tetra world)) "left"))]
                     [(key=? "right" key) (make-tetra (posn-move (tetra-center (world-tetra world)) "right") (bset-move (tetra-blocks (world-tetra world)) "right"))]
                     [else (world-tetra world)])
              (world-pile world)))

;; bset-move moves a bset
;; bset string -> bset
(define (bset-move bset direction)
  (cond [(empty? bset) empty]
        [(string=? direction "right") (cons (make-block (+ (block-x (first bset)) 1) 
                                                 (block-y (first bset)) 
                                                 (block-color (first bset))) 
                                     (bset-move (rest bset) direction))]
         [(string=? direction "left") (cons (make-block (- (block-x (first bset)) 1) 
                                                 (block-y (first bset)) 
                                                 (block-color (first bset))) 
                                     (bset-move (rest bset) direction))]))
;; posn-move moves a tetras posn
;; posn -> posn
(define (posn-move posn direction)
  (cond [(string=? direction "right") (make-posn (+ (posn-x posn) 1) (posn-y posn))] 
        [(string=? direction "left") (make-posn (- (posn-x posn) 1) (posn-y posn))]))

;; tetra-rotate moves all the blocks in a tetra as if theyre rotating
;; tetra -> tetra

;; gravity moves all blocks down
;; tetra->tetra
(define (gravity tetra)
  (make-tetra 
        (make-posn (posn-x (tetra-center tetra)) (+ 1 (posn-y (tetra-center tetra))))
        (bset-gravity (tetra-blocks tetra))))

;;block gravity takes a bset and applies +1 to each Y
;; bset->bset
(define (bset-gravity bset)
  (cond [(empty? bset) empty]
        [(block? (first bset)) (cons (make-block (block-x (first bset)) 
                                                 (+ 1 (block-y (first bset))) 
                                                 (block-color (first bset))) 
                                     (bset-gravity (rest bset)))]))


(check-expect (gravity tetra-o) (make-tetra (make-posn (+ INITIAL-X-1 .5) (+ INITIAL-Y-1 1.5)) 
                            (list (make-block INITIAL-X-1 (+ 1 INITIAL-Y-1) 'green) 
                                  (make-block INITIAL-X (+ 1 INITIAL-Y-1) 'green)
                                  (make-block INITIAL-X-1 (+ 1 INITIAL-Y-2) 'green) 
                                  (make-block INITIAL-X (+ 1 INITIAL-Y-2) 'green))))

;; block-rotate-ccw : Posn Block -> Block
;; Rotate the block 90 counterclockwise around the posn.
;; C = CENTER, B = BLOCK
(define (block-rotate-ccw c b)
  (make-block (+ (posn-x c)
                 (- (posn-y c)
                    (block-y b)))
              (+ (posn-y c)
                 (- (block-x b)
                    (posn-x c)))
              (block-color b)))


;;;;;;;;;;;;;;;;;;;;;ON TICK;;;;;;;;;;;;;;;;

;; update-world updates the world
;; checks for collisions, lose state, score, applies gravity, 
(define (update-world world) 
  (make-world (gravity (world-tetra world)) 
              (world-pile world)))

;; gravity 

(big-bang INITIAL-WORLD
          (to-draw draw-world)
          (on-tick update-world TICKRATE)
          (on-key key)
        ;  (stop-when game-over)
        )