;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname assignment1-snakegame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Assignment 1
;; Name: Carlee Foster

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

;; main : Number -> Game
;; Launches a game of "Bug" played at given tick rate
;; Example: (main 1/10)
(define (main r)
  (big-bang G0
    [to-draw game-draw]
    [on-tick game-advance r]
    [on-key game-handle-key]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A Game is a (make-game Bug Food)
(define-struct game (bug food))

;; A Bug is a (make-bug Dir Posn)
(define-struct bug (dir posn))

;; A Food is a Posn

;; A Posn is a (make-posn Integer Integer)

;; A Dir is one of:
;; - "left"
;; - "right"
;; - "up"
;; - "down"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defined Constants

(define PX/U 20) ; pixels per unit
(define WIDTH 20)  ; units
(define HEIGHT 20) ; units

(define G0 ; an initial game state
  (make-game (make-bug "up"
                       (make-posn (quotient WIDTH 2)
                                  (quotient HEIGHT 2)))
             (make-posn 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game functions

;; game-handle-key : Game KeyEvent -> Game
;; Handle a key event in this game
(define (game-handle-key g ke)
  ; stub
  g)

;; game-advance : Game -> Game
;; Adance the bug, maybe eating the food
(define (game-advance g)
  ; stub key
  g)

;; game-draw : Game -> Scene
;; Render the game as a scene
(define (game-draw g)
  (empty-scene (* WIDTH PX/U)
               (* HEIGHT PX/U)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bug functions

;; bug-change-dir : Bug Dir -> Bug
;; Change bug's direction to given one
(define (bug-change-dir b d)
    (cond [(string=? d "left") (posn-advance-left (make-posn (posn-x b) (posn-y b)))]
        [(string=? d "right") (posn-advance-right (make-posn (posn-x b) (posn-y b)))]
        [(string=? d "up") (posn-advance-up (make-posn (posn-x b)(posn-y b)))]
        [(string=? d "down") (posn-advance-down (make-posn (posn-x b) (posn-y b)))]
        [else
         (make-posn (posn-x b) (posn-y b))]))

(check-expect (bug-change-dir (make-posn 14 16) "left") (posn-advance-left (make-posn 14 16)))
(check-expect (bug-change-dir (make-posn 14 16) "right") (posn-advance-right (make-posn 14 16)))
(check-expect (bug-change-dir (make-posn 14 16) "up") (posn-advance-up (make-posn 14 16)))
(check-expect (bug-change-dir (make-posn 14 16) "down") (posn-advance-down (make-posn 14 16)))

;; bug-advance : Bug -> Bug
;; Advance bug in its current direction, but not past board boundaries

(define (bug-advance b)
  (make-bug (bug-dir b) (posn-advance (bug-posn b) (bug-dir b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Posn functions

;; posn-advance : Posn Dir -> Posn
;; Advance the posn in given direction, but not past boundaries
(define (posn-advance d p)
  (cond [(string=? d "left") (posn-advance-left (make-posn (posn-x p) (posn-y p)))]
        [(string=? d "right") (posn-advance-right (make-posn (posn-x p) (posn-y p)))]
        [(string=? d "up") (posn-advance-up (make-posn (posn-x p)(posn-y p)))]
        [(string=? d "down") (posn-advance-down (make-posn (posn-x p) (posn-y p)))]
        [else
         (make-posn (posn-x p) (posn-y p))]))

(check-expect (posn-advance "left" (make-posn -1 14)) (posn-advance-left (make-posn 0 14)))
(check-expect (posn-advance "right" (make-posn 21 17)) (posn-advance-right (make-posn 20 17)))
(check-expect (posn-advance "up" (make-posn 2 -1)) (posn-advance-up (make-posn 2 0)))
(check-expect (posn-advance "down" (make-posn 4 22)) (posn-advance-down (make-posn 4 20)))
(check-expect (make-posn 2 7) (make-posn 2 7))



;; posn-advance-left : Posn -> Posn
;; Advance the posn toward left, but not past left boundary
(define (posn-advance-left p)
  (if (> (posn-x p) 0)
      (make-posn (sub1 (posn-x p)) (posn-y p))
      (make-posn 0 (posn-y p))))

(check-expect (posn-advance-left (make-posn -1 17)) (make-posn 0 17))
(check-expect (posn-advance-left (make-posn 19 14)) (make-posn 18 14))
(check-expect (posn-advance-left (make-posn 20 15)) (make-posn 19 15))

;; posn-advance-right : Posn -> Posn
;; Advance the posn toward right, but not past right boundary
(define (posn-advance-right p)
  (if (< (posn-x p) WIDTH)
      (make-posn (add1 (posn-x p)) (posn-y p))
      (make-posn WIDTH (posn-y p))))

(check-expect (posn-advance-right (make-posn 6 9)) (make-posn 7 9))
(check-expect (posn-advance-right (make-posn 21 19)) (make-posn 20 19))
(check-expect (posn-advance-right (make-posn 8 20)) (make-posn 9 20))

;; posn-advance-up : Posn -> Posn
;; Advance the posn toward top, but not past top boundary
(define (posn-advance-up p)
    (if (> (posn-y p) 0)
      (make-posn (posn-x p) (sub1 (posn-y p)))
      (make-posn (posn-x p) 0)))

(check-expect (posn-advance-up (make-posn 6 9)) (make-posn 6 8))
(check-expect (posn-advance-up (make-posn 11 21)) (make-posn 11 20))
(check-expect (posn-advance-up (make-posn 4 -1)) (make-posn 4 0))

;; posn-advance-down : Posn -> Posn
;; Advance the posn toward bottom, but not past bottom boundary
(define (posn-advance-down p)
  (if (< (posn-y p) HEIGHT)
      (make-posn (posn-x p) (add1 (posn-y p)))
      (make-posn (posn-x p) HEIGHT)))

(check-expect (posn-advance-down (make-posn 18 19)) (make-posn 18 20))
(check-expect (posn-advance-down (make-posn 10 21)) (make-posn 10 20))
(check-expect (posn-advance-down (make-posn 12 14)) (make-posn 12 15))

;; posn=? : Posn Posn -> Boolean
;; Are the two posns at the same position?
(define (posn=? p1 p2)
  (cond [(and (= (posn-x p1) (posn-x p2))
         (= (posn-y p1) (posn-y p2))) #true]
         [ else
           #false]))

(check-expect (posn=? (make-posn 18 19) (make-posn 18 19)) #true)
(check-expect (posn=? (make-posn 17 19) (make-posn 18 20)) #false)
(check-expect (posn=? (make-posn 12 10) (make-posn 16 10)) #false)

;; posn-draw-on : Posn Color Scene -> Scene
;; Draw a colored circled at given posn on scene
(define (posn-draw-on p color scn)
  (place-image (circle (* 1/2 PX/U) "solid" color)
               (+ (* (posn-x p) PX/U) (* 1/2 PX/U))
               (+ (* (posn-y p) PX/U) (* 1/2 PX/U))
               scn))
