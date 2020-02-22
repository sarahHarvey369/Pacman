;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw04text) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Sarah Harvey
;hw04
;https://www.radford.edu/~itec380/2020spring-ibarland/Homeworks/hw04/

(require 2htdp/image)
(require 2htdp/universe)

;Design Recipe:
;1. Make data definition
;2. Give some examples of that data type
;3. Template:

;Data Definition:
(define-struct team (name offense defense))
;   make-team : string, real, real -> boolean

;Examples of the data
(define team1 (make-team "t1" 80 50))
(define team2 (make-team "t2" 60 90))
(define team3 (make-team "Lakers" 113.4 106.8))

; func-for-team : team -> ????
; Return ???
(define (func-for-team a-team)
  (... (team-name a-team)
   ... (team-offense a-team)
   ... (team-defense a-team)))

;team>? : team, team -> boolean
;Return a boolean reprsenting if the first team, `team1` is greater than the second team, `team2`.
;One team is greater if their offense is higher than the other's defense, and it's defense is higher than the other's offense.
(define (team>? team1 team2)
  (and (> (team-offense team1)
          (team-defense team2))
       (> (team-defense team1)
          (team-offense team2))))

(check-expect (team>? (make-team "t1" 80 50) (make-team "t2" 60 90)) #false)
(check-expect (team>? (make-team "t1" 90 60) (make-team "t2" 50 80)) #true)
(check-expect (team>? (make-team "t1" 80 60) (make-team "t2" 50 90)) #false)
(check-expect (team>? (make-team "t1" 90 50) (make-team "t2" 60 80)) #false)
(check-expect (team>? (make-team "t1" 80 50) (make-team "t2" 60 90)) #false)
(check-expect (team>? (make-team "t1" 80 60) (make-team "t2" 60 80)) #false)
(check-expect (team>? (make-team "t1" 80 50) (make-team "t2" 60 80)) #false)
(check-expect (team>? (make-team "t1" 80 60) (make-team "t2" 50 80)) #false)
(check-expect (team>? (make-team "t1" 80 50) (make-team "t2" 50 90)) #false)
(check-expect (team>? (make-team "t1" 90 60) (make-team "t2" 60 80)) #false)

;--------------------------------------------------------------------------------------------------------------

(define speed 3)



;Data Definition: A "direction" is:
;   A string representing the key-event of four possible directions: left, right, up, and down.

;Give some examples:
    "left"
    "right" 
    "up"
    "down"

;Template:
;direction : key-event? -> ???
(define (func-for-direction direction)
  (cond [(key=? direction "left") ...]
        [(key=? direction "right") ...]
        [(key=? direction "up") ...]
        [(key=? direction "down") ...]))



;Data Definition:
(define-struct position (x y))
;   make-position : real, real -> position

;Examples of the data:
(define position1 (make-position 25 80))
(define position50 (make-position 0 62))

;Template:
;func-for-position : position -> ???
(define (func-for-position a-pos)
  (... (position-x a-pos)
   ... (position-y a-pos)))    



;Data Definition: 
(define-struct displacement (dx dy))
;   make-displacement : real, real -> displacement

;Examples of the data:
(define displacement1 (make-displacement 1 0))
(define displacement2 (make-displacement 0 1))
(define displacement3 (make-displacement -1 0))
(define displacement4 (make-displacement 0 -1))

;Template:
;func-for-displacement : displacement -> ???
(define (func-for-displacement a-displacement)
  (... (displacement-dx a-displacement)
   ... (displacement-dy a-displacement)))



;dx-dy : direction -> displacement
;Returns a representation of dx and dy, `displacement` given a direction, `direction`.
(define (dx-dy direction)
  (cond [(key=? direction "up") (make-displacement 0 -1)]
        [(key=? direction "down") (make-displacement 0 1)]
        [(key=? direction "left") (make-displacement -1 0)]
        [(key=? direction "right") (make-displacement 1 0)]))

(check-expect (dx-dy "up") (make-displacement 0 -1))
(check-expect (dx-dy "down") (make-displacement 0 1))
(check-expect (dx-dy "left") (make-displacement -1 0))
(check-expect (dx-dy "right") (make-displacement 1 0)) 



;Data Definition:
(define-struct pacman (direction age position))
;   make-pacman : key-event?, tick, position?, real -> pacman
; direction: (defined above)
; age: an amount of time represented by ticks; 1 tick per two frames
; position: (defined above)

;Examples of the data:
(define pacman-start (make-pacman "left" 5 (make-position 25 80)))
(define pacman-lost (make-pacman "up" 1000 (make-position 0 62)))

;Template:
;func-for-pacman : pacman -> ???
(define (func-for-pacman a-pacman)
  (... (pacman-direction a-pacman)
   ... (pacman-age a-pacman)
   ... (pacman-position a-pacman)))



;pos : position?, string -> position?
;Returns a x y position? given a postion, `object-position` and a string representing a direction, `object-direction`
(define (pos object-position object-direction)
  (make-position (max 0 (+ (position-x object-position)
                           (* speed (displacement-dx (dx-dy object-direction)))))
                 (max 0 (+ (position-y object-position)
                           (* speed (displacement-dy (dx-dy object-direction)))))))

(check-expect (pos (make-position 0 0) "up") (make-position 0 0))
(check-expect (pos (make-position 62 0) "down") (make-position 62 3))
(check-expect (pos (make-position 39 21) "left") (make-position 36 21))
(check-expect (pos (make-position 39 21) "right") (make-position 42 21))



;glide-pacman : pacman -> pacman
;Return a pacman one tick of time later
(define (glide-pacman pacman)
  (make-pacman (pacman-direction pacman)
               (+ 1 (pacman-age pacman))
               (pos (pacman-position pacman) (pacman-direction pacman))))

(check-expect (glide-pacman (make-pacman "up" 0 (make-position 0 0))) (make-pacman "up" 1 (make-position 0 0)))
(check-expect (glide-pacman (make-pacman "up" 5 (make-position 25 60))) (make-pacman "up" 6 (make-position 25 57)))
(check-expect (glide-pacman (make-pacman "up" 10 (make-position 25 0))) (make-pacman "up" 11 (make-position 25 0)))

(check-expect (glide-pacman (make-pacman "down" 0 (make-position 0 0))) (make-pacman "down" 1 (make-position 0 3)))
(check-expect (glide-pacman (make-pacman "down" 1 (make-position 62 0))) (make-pacman "down" 2 (make-position 62 3)))
(check-expect (glide-pacman (make-pacman "down" 21 (make-position 68 3))) (make-pacman "down" 22 (make-position 68 6)))

(check-expect (glide-pacman (make-pacman "left" 0 (make-position 0 0))) (make-pacman "left" 1 (make-position 0 0)))
(check-expect (glide-pacman (make-pacman "left" 63 (make-position 15 23))) (make-pacman "left" 64 (make-position 12 23)))
(check-expect (glide-pacman (make-pacman "left" 100 (make-position 39 21))) (make-pacman "left" 101 (make-position 36 21)))

(check-expect (glide-pacman (make-pacman "right" 0 (make-position 0 0))) (make-pacman "right" 1 (make-position 3 0)))
(check-expect (glide-pacman (make-pacman "right" 121 (make-position 62 41))) (make-pacman "right" 122 (make-position 65 41)))
(check-expect (glide-pacman (make-pacman "right" 28 (make-position 39 21))) (make-pacman "right" 29 (make-position 42 21)))




;Data Definition:
(define-struct ghost (direction age position))
;   make-ghost : key-event?, tick, position? -> ghost
; direction: (defined above)
; age: an amount of time represented by ticks; 1 tick per two frames
; position: (defined above)

;Examples of the data:
(define ghost-one (make-ghost "left" 5 (make-position 25 80)))
(define ghost-two (make-ghost "up" 1000 (make-position 0 62)))

;Template:
;func-for-ghost : ghost -> ???
(define (func-for-ghost a-ghost)
  (... (ghost-direction a-ghost)
   ... (ghost-age a-ghost)
   ... (ghost-position a-ghost)))



;glide-ghost : ghost -> ghost
;Returns a ghost one tick of time later
(define (glide-ghost ghost)
  (glide-pacman ghost))

(check-expect (glide-ghost (make-pacman "up" 0 (make-position 0 0))) (make-pacman "up" 1 (make-position 0 0)))
(check-expect (glide-ghost (make-pacman "up" 5 (make-position 25 60))) (make-pacman "up" 6 (make-position 25 57)))
(check-expect (glide-ghost (make-pacman "up" 10 (make-position 25 0))) (make-pacman "up" 11 (make-position 25 0)))

(check-expect (glide-ghost (make-pacman "down" 0 (make-position 0 0))) (make-pacman "down" 1 (make-position 0 3)))
(check-expect (glide-ghost (make-pacman "down" 1 (make-position 62 0))) (make-pacman "down" 2 (make-position 62 3)))
(check-expect (glide-ghost (make-pacman "down" 21 (make-position 68 3))) (make-pacman "down" 22 (make-position 68 6)))

(check-expect (glide-ghost (make-pacman "left" 0 (make-position 0 0))) (make-pacman "left" 1 (make-position 0 0)))
(check-expect (glide-ghost (make-pacman "left" 63 (make-position 15 23))) (make-pacman "left" 64 (make-position 12 23)))
(check-expect (glide-ghost (make-pacman "left" 100 (make-position 39 21))) (make-pacman "left" 101 (make-position 36 21)))

(check-expect (glide-ghost (make-pacman "right" 0 (make-position 0 0))) (make-pacman "right" 1 (make-position 3 0)))
(check-expect (glide-ghost (make-pacman "right" 121 (make-position 62 41))) (make-pacman "right" 122 (make-position 65 41)))
(check-expect (glide-ghost (make-pacman "right" 28 (make-position 39 21))) (make-pacman "right" 29 (make-position 42 21)))



;Data Definition:
(define-struct dot (radius position))
;   make-dot : positive real, position? -> d

;Examples of the data:
(define dot-one (make-dot 3 (make-position 25 9)))
(define dot-two (make-dot 5 (make-position 43 72)))



;Data Definition:
(define-struct wall (width height position))
;   make-wall : non-negative real, non-negative real, list of positions -> wall

;Examples of the data:
(define wall-one (make-wall 80 80 (make-position 0 0)))
(define wall-two (make-wall 45 45 (make-position 10 20)))



;border : positive real -> image
(define (border side-length)
  (rectangle side-length side-length "solid" "black"))

(check-expect (border 300) (rectangle 300 300 "solid" "black"))
(check-expect (border 50) (rectangle 50 50 "solid" "black"))



;draw-dot : dot, image -> image
;Returns an image of a dot given a dot, `dot` and and image `image`
(define (draw-dot dot image)
  (place-image
   (circle (dot-radius dot) "solid" "white")
   (position-x (dot-position dot)) (position-y (dot-position dot))
   image))

(check-expect (draw-dot (make-dot 3 (make-position 50 50)) (border 500)) (place-image (circle 3 "solid" "white") 50 50 (border 500)))
(check-expect (draw-dot (make-dot 3 (make-position 0 50)) (border 100)) (place-image (circle 3 "solid" "white") 0 50 (border 100)))



;draw-wall : wall, image -> image
;Returns an rectangle image, `wall`, placed on another image, `image`
(define (draw-wall wall image)
  (place-image
   (rectangle (wall-width wall) (wall-height wall) "solid" "blue")
   (position-x (wall-position wall)) (position-y (wall-position wall))
   image))

(check-expect (draw-wall (make-wall 10 20 (make-position 50 50)) (border 100)) .)
(check-expect (draw-wall (make-wall 5 20 (make-position 0 0)) (border 100)) .)



;draw-ghost : ghost, image -> image
;Returns a ghost image placed on another image, `image` based on `pacman` position
(define (draw-ghost ghost image)
  (place-image
   (crop/align "center" "top" 16 16 (ellipse 16 32 "solid" "orange"))
   (position-x (ghost-position ghost)) (position-y (ghost-position ghost))
   image))
(check-expect (draw-ghost (make-ghost "left" 5 (make-position 25 40)) (draw-dot (make-dot 3 (make-position 50 50)) (border 100))) .)

;pacman-image :
;Returns a pacman image given the rotation angle and direction of rotation
(define (pacman-img angle dir x y)
  (place-image
   (rotate angle (crop/align dir "top" 8 8 (circle 8 "solid" "black")))
   x y
   (circle 8 "solid" "yellow")))


(define (pacman-mouth-direction pacman)
  (cond [(eq? (pacman-direction pacman) "left") (pacman-img 45 "left" 4 8)]
        [(eq? (pacman-direction pacman) "right") (pacman-img -45 "right" 12 8)]
        [(eq? (pacman-direction pacman) "up") (pacman-img 45 "right" 8 4)]
        [(eq? (pacman-direction pacman) "down") (pacman-img 135 "left" 8 12)]))
        


;pacman-look : real -> image
;Returns a circle image based on if an age, `age` is even or odd
(define (pacman-look pacman)
  (cond [(odd? (pacman-age pacman)) (circle 8 "solid" "yellow")]
        [(even? (pacman-age pacman)) (pacman-mouth-direction pacman)]))

(check-expect (pacman-look (make-pacman "up" 1 (make-position 0 0))) (circle 8 "solid" "yellow"))
(check-expect (pacman-look (make-pacman "down" 1 (make-position 0 0))) (circle 8 "solid" "yellow"))
(check-expect (pacman-look (make-pacman "right" 1 (make-position 0 0))) (circle 8 "solid" "yellow"))
(check-expect (pacman-look (make-pacman "left" 1 (make-position 0 0))) (circle 8 "solid" "yellow"))
(check-expect (pacman-look (make-pacman "left" 0 (make-position 0 0))) (place-image
                                                                        (rotate 45 (crop/align "left" "top" 8 8 (circle 8 "solid" "black")))
                                                                        4 8
                                                                        (circle 8 "solid" "yellow")))
(check-expect (pacman-look (make-pacman "right" 0 (make-position 0 0))) (place-image
                                                                        (rotate -45 (crop/align "right" "top" 8 8 (circle 8 "solid" "black")))
                                                                        12 8
                                                                        (circle 8 "solid" "yellow")))
(check-expect (pacman-look (make-pacman "down" 0 (make-position 0 0))) (place-image
                                                                        (rotate 135 (crop/align "left" "top" 8 8 (circle 8 "solid" "black")))
                                                                        8 12
                                                                        (circle 8 "solid" "yellow")))
(check-expect (pacman-look (make-pacman "up" 0 (make-position 0 0))) (place-image
                                                                        (rotate 45 (crop/align "right" "top" 8 8 (circle 8 "solid" "black")))
                                                                        8 4
                                                                        (circle 8 "solid" "yellow")))



;draw-pacman : pacman, image -> image
;Returns a pacman image placed on another image, `image` based on `pacman` position
(define (draw-pacman pacman image)
  (place-image
   (pacman-look pacman)
   (position-x (pacman-position pacman)) (position-y (pacman-position pacman))
   image))
(check-expect (draw-pacman (make-pacman "right" 5 (make-position 25 80)) (draw-ghost (make-ghost "right" 5 (make-position 25 40)) (draw-dot (make-dot 3 (make-position 50 50)) (border 100)))) .)
(check-expect (draw-pacman (make-pacman "right" 6 (make-position 25 80)) (draw-ghost (make-ghost "right" 6 (make-position 25 40)) (draw-dot (make-dot 3 (make-position 50 50)) (border 300)))) .)



;pacman-handle-key : pacman -> pacman
;Returns a new pacman that has responded to the keypress
(define (pacman-handle-key pacman key)
  (cond [(key=? key "up") (make-pacman "up" (pacman-age pacman) (pacman-position pacman))]
        [(key=? key "down") (make-pacman "down" (pacman-age pacman) (pacman-position pacman))]
        [(key=? key "left") (make-pacman "left" (pacman-age pacman) (pacman-position pacman))]
        [(key=? key "right") (make-pacman "right" (pacman-age pacman) (pacman-position pacman))]))

;draw-world : pacman -> image
;Returns an image with a packman in it
(define (draw-world pacman)
  (draw-pacman pacman (border 800)))

(require "overlap.rkt")
;pacman-collide-ghost? : pacman, ghost -> boolean?
;Returns
(define (pacman-collide-ghost pacman ghost)
  (overlap? (position-x (pacman-position pacman)) (position-y (pacman-position pacman)) 16 16 (position-x (ghost-position ghost)) (position-y (ghost-position ghost)) 8 8))


(big-bang (make-pacman "left" 0 (make-position 150 200))
    [on-key  pacman-handle-key]
    [on-tick glide-pacman]
    [to-draw draw-world])






