extensions [ nw ]

globals [
  world-offset
  global-street-distance
  global-street-width
  global-non-wall-color

  initial-state
]

;; Beacons are nodes in our network that communicate
;; the shortest path to the movers that need to
;; reach their destination
breed [beacons beacon]
;; These turtles are used for street drawing
breed [street-drawers street-drawer]

;; Streets are links between beacons with certain capacity
undirected-link-breed [streets street]
directed-link-breed [directed-streets directed-street]

streets-own [
  weight
  street-width
]

directed-streets-own [
  weight
  street-width
]

street-drawers-own [
  drawer-destination
  drawer-width
]

beacons-own [
  intersection-width
  intersection-height
  intersection-radius
  interest-point?
  entry-point?
  entry-ratios
  entry-infinity?
  entry-limit
  exit-point?

  entry-percentages
  exit-percentages

  entry-rate
  exit-rate
]

;; Patches can be wall so they are now walkable
patches-own [
  wall
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALL THE CONTROL AND SETUP SECTION HAS BEEN MOVED TO THE BOTTOM OF THE FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTROL SECTION
;; ===============

to setup
  ;; First we clear everything
  clear-all
  reset-ticks

  ;; Set the default shapes and the global variables
  default-configuration

  ;; After that we can generate our map
  generate-map
  tick
end

to export
  export-world export-filename
end

;; Used to set the defaults and globals
to default-configuration
  set-default-shape beacons "box"
  set world-offset 10
  set global-street-distance 15
  set global-street-width 3
  set global-non-wall-color 9
end

;; HELPER TASKS
;; ============

;; Set all the patches to walls, and then exlude those
;; with correct color
to make-patches-non-wall
  ask patches [set wall true]
  ask patches with [pcolor = global-non-wall-color] [set wall false]
end

;; Ask the agent to paint the list of patches around him
;; contained in a list-of-patches list
to paint-patches [agent list-of-patches]
  ask agent [paint-agents patches at-points list-of-patches]
end

to paint-patches-in-radius [agent-radius]
  ask patches in-radius agent-radius [ set pcolor global-non-wall-color ]
end

;; This is a helper procedure, used to set the color of a set of patches.
to paint-agents [agents]
  ask agents [ set pcolor global-non-wall-color ]
end

;; This reporter returns a list of patch positions relative to [0 0]
;; of given width and height. Should be used to draw squares and streets
;; iteratively
to-report get-square-patches [square-width square-height]
  let square-patches []
  foreach n-values square-height [?] [
    let current-row ?
    foreach n-values square-width [?] [
      set square-patches sentence square-patches (list map-to-tuple current-row ?)
    ]
  ]
  report square-patches
end

;; Given a set of relative square patches, with their width and height
;; return a centralized set of patches around the relative [0 0] position
to-report centralize-patches [square-patches square-width square-height]
  let diff-width floor (square-width / 2)
  let diff-height floor (square-height / 2)
  set square-patches map [list (item 0 ? - diff-height) (item 1 ? - diff-width)] square-patches
  report square-patches
end

to-report map-to-tuple [elem1 elem2]
  report (list elem1 elem2)
end

;; STREET DRAWERS
;; ==============

;; Draw squares takes all the beacons and draws the square
;; base on their local variables of width and height
to draw-squares
  ask beacons [
    ifelse intersection-width > 0 and intersection-height > 0
    [ let patches-to-paint centralize-patches (get-square-patches intersection-width intersection-height) intersection-width intersection-height
      paint-patches self patches-to-paint ]
    [ paint-patches-in-radius intersection-radius ]

  ]
end

to draw-streets
  generate-street-drawers
  actually-draw-streets
  ask street-drawers [die]
end

to generate-street-drawers
  ask beacons [
    let all-streets (link-set my-streets my-out-directed-streets)
    ask all-streets [
      let d-destination other-end
      let d-width street-width
      ask myself [
        hatch-street-drawers 1 [
          set drawer-destination d-destination
          set drawer-width d-width
        ]
      ]
    ]
  ]
end

to actually-draw-streets
  ask street-drawers [
    let patches-to-paint centralize-patches (get-square-patches drawer-width drawer-width) drawer-width drawer-width
    while [destination-reached? self drawer-destination] [
      paint-patches self patches-to-paint
      drawer-move-ahead "straight" drawer-destination
    ]
  ]
end

to drawer-move-ahead [moving-style move-destination]
  if moving-style = "diagonal" [
    face move-destination
    move-to next-patch
  ]
  if moving-style = "straight" [
    face move-destination
    fd 1
  ]
end

to-report destination-reached? [agent agent-destination]
  ifelse [patch-here] of agent != [patch-here] of agent-destination
  [ report true ]
  [ report false ]
end

;; report next patch of a von neumann neighborhood
to-report next-patch  ;; turtle procedure
  if heading < towardsxy (pxcor + 0.5) (pycor + 0.5)
    [ report patch-at 0 1 ]
  if heading < towardsxy (pxcor + 0.5) (pycor - 0.5)
    [ report patch-at 1 0 ]
  if heading < towardsxy (pxcor - 0.5) (pycor - 0.5)
    [ report patch-at 0 -1 ]
  if heading < towardsxy (pxcor - 0.5) (pycor + 0.5)
    [ report patch-at -1 0 ]
  report patch-at 0 1
end

to make-beacon-normal
  set entry-point? false
  set exit-point? false
  set interest-point? false
  set entry-percentages []
  set exit-percentages []
end

to make-beacon-entry
  set entry-point? true
  set exit-point? false
  set interest-point? false
  set entry-percentages []
end

to make-beacon-exit
  set entry-point? false
  set exit-point? true
  set interest-point? false
  set exit-percentages []
end

to generate-interest-points
  ask n-of 2 beacons with [entry-point? = false and exit-point? = false] [
    set interest-point? true
  ]
end

to generate-map
  let new-world-width 2 * world-offset + 20
  let new-world-height 2 * world-offset + 20
  resize-world 0 new-world-width 0 new-world-height
  generate-beacons
  generate-interest-points
  connect-beacons
  draw-squares
  draw-streets
  make-patches-non-wall
end

to generate-beacons
  ask patch (world-offset + 0) (world-offset + 0) [sprout-beacons 1 [
    make-beacon-entry
    set intersection-width 5
    set intersection-height 5
    set intersection-radius 3
    set entry-percentages lput [0 0.4] entry-percentages
    set entry-percentages lput [1 0.6] entry-percentages
    set entry-rate 0.05
    set entry-limit 30
    set entry-infinity? false
  ]]
  ask patch (world-offset + 0) (world-offset + 20) [sprout-beacons 1 [
    make-beacon-normal
    set intersection-width 5
    set intersection-height 5
    set intersection-radius 3
  ]]
  ask patch (world-offset + 20) (world-offset + 0) [sprout-beacons 1 [
    make-beacon-normal
    set intersection-width 5
    set intersection-height 5
    set intersection-radius 3
  ]]
  ask patch (world-offset + 20) (world-offset + 20) [sprout-beacons 1 [
    make-beacon-exit
    set intersection-width 5
    set intersection-height 5
    set intersection-radius 3
    set exit-percentages lput [0 0.5] exit-percentages
    set exit-percentages lput [1 0.5] exit-percentages
    set exit-rate 0.05
  ]]
end
to connect-beacons
  ask beacons-on patch (world-offset + 0) (world-offset + 0) [
    create-streets-with beacons-on patch (world-offset + 0) (world-offset + 20) [
      set weight 3
      set street-width 3]]
  ask beacons-on patch (world-offset + 0) (world-offset + 20) [
    create-directed-streets-to beacons-on patch (world-offset + 20) (world-offset + 20) [
      set weight 3
      set street-width 3]]
  ask beacons-on patch (world-offset + 0) (world-offset + 0) [
    create-streets-with beacons-on patch (world-offset + 20) (world-offset + 0) [
      set weight 3
      set street-width 3]]
  ask beacons-on patch (world-offset + 20) (world-offset + 0) [
    create-streets-with beacons-on patch (world-offset + 20) (world-offset + 20) [
      set weight 3
      set street-width 3]]
end

to imposta-colori
  ask beacon 0[set color yellow]
  ask beacon 1[set color  104]
  ask beacon 2 [set color green]
  ask beacon 3[set color red]
end
@#$#@#$#@
GRAPHICS-WINDOW
285
10
787
533
-1
-1
12.0
1
10
1
1
1
0
0
0
1
0
40
0
40
0
0
1
ticks
30.0

BUTTON
27
41
100
74
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
196
162
275
195
NIL
export
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
30
94
275
154
export-filename
grid_x_poi_y_01.csv
1
0
String

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
