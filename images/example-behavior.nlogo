extensions [nw table]

;; Beacons are nodes in our network that comunicate
;; the shortest path to the movers that need to
;; reach their destination
breed [beacons beacon]
;; These turtles are used for street drawing
breed [street-drawers street-drawer]

;; Movers are turtles that need to reach certain destinations
breed [movers mover]

;; Streets are links between beacons with certain capacity
undirected-link-breed [streets street]
directed-link-breed [directed-streets directed-street]

movers-own [
  destination-beacon ;; current destination moving towards
  destination-list ;; list of all my locations
  destination-order ;; Table with strategies to calculate my next destination
  destination-reached ;; whether I've reached the current destination
  current-beacon ;; current beacon on my path
  previous-beacon ;; beacon that I'm coming from
  undesired-street ;; street that is not included in my path, since i got stuck there
  speed
  mover-behavior
  should-move? ;; used when calculating the next patch
  patience ;; for how long can I be stuck before changing my direction

  movers-data ;; used for outputing some results
]

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
  entry-ratios
  entry-infinity?
  entry-limit
  entry-point?
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

globals [
  ;; these need to be redefined
  global-crowd-max-at-patch

  ;; global tables
  destination-ordering
  behaviors-map

  ;; Needed for import to work
  world-offset
  global-street-distance
  global-street-width
  global-non-wall-color
  grid-size
  n-interest-points
  export-filename

  ;; Needed for results collection
  global-state
  global-state-grid
  global-state-x
  global-state-y
  global-movers-results
  global-list-interest-points
  global-list-exit-points

  ;;list containing initial state parameters
  initial-state
]

;; IMPORT AND DEFAULTS
;; ===================

to setup
  import-world import-filename
  default-configuration
end

;; GO PROCEDURE
;; ============

to go
  update-global-state
  if use-exits = true [ old-movers-leave ]
  if use-entries = true [ new-movers-enter ]
  move
  tick
end

to old-movers-leave
  let temp-exit-rate 0
  ask beacons with [exit-point? = true] [
    let exit-beacon self
    set temp-exit-rate exit-rate
    ask patches in-radius intersection-radius [
      ask movers-here [
        if random-float 1 < temp-exit-rate and empty? destination-list [
          ;; generate final data and put them in a global var
          movers-data-collect exit-beacon
          movers-data-move-to-global
          die ] ]
    ]
  ]
end

to new-movers-enter
  let temp-entry-rate 0
  ask beacons with [entry-point? = true and (entry-infinity? = true or entry-limit > 0)] [
    set temp-entry-rate entry-rate
    ask patches in-radius intersection-radius with [count movers-here < global-crowd-max-at-patch] [
      if random-float 1 < temp-entry-rate [
        generate-new-mover
        ask myself [set entry-limit entry-limit - 1]
      ]
    ]
  ]
end

to generate-new-mover
  sprout-movers 1 [
    set current-beacon min-one-of beacons [distance myself]
    set mover-behavior get-random-mover-behaviour [entry-percentages] of current-beacon
    standard-mover-settings
  ]
end

to standard-mover-settings
  set previous-beacon current-beacon
  set undesired-street false
  set speed 0.5
  set patience global-patience

  set destination-list table:get behaviors-map mover-behavior
  set destination-list sort destination-list

  set destination-order "minDistance"
  ifelse not empty? destination-list
    [run table:get destination-ordering destination-order]
    [set destination-beacon min-one-of (beacons with [exit-point? = true]) [distance myself]]

  set destination-reached false

  ;; populate the initial map for the output data
  movers-data-setup

  ;; set transparency
  set color [color] of destination-beacon
  ifelse is-list? color
    [ set color lput 100 sublist color 0 3 ]
    [ set color lput 100 extract-rgb color ]
end

;; Given a list of behaviours and their probabilities
;; returns a random behaviour wrt to those probabilities
to-report get-random-mover-behaviour [iter-list]
  let random-dice random-float 1
  let acc 0
  foreach iter-list [
    set acc acc + (item 1 ?)
    if random-dice < acc
      [ report item 0 ? ]
  ]
end

;; MOVE PROCEDURE
;; ==============

;; Just a standard task that calls all the right procedures
to move
  update-path
  update-next-patch
  ask movers with [should-move? = true] [ fd speed ]
  ask movers with [should-move? = false] [ set patience patience - 1]
  ask movers with [patience <= 0] [
    orient-random-mover self
    set patience random (3 + global-patience)
  ]
end
;; Check if I've reached the beacons and update my path,
;; otherwise keep going towards the current-beacon
to update-path
  ask movers [
    let next-beacon current-beacon
    let current-mover self

    ;; have I reached the current beacon?
    ask current-beacon [
      if member? [patch-here] of myself patches in-radius intersection-radius [
        ;; set the previous beacon to the current-one, since we have reached it
        ask myself [set previous-beacon current-beacon]

        ;; Control if this beacon is in my destination list and remove it
        check-if-involuntary-destination current-mover self

        ;; calculate the full path to my destination
        nw:set-context (beacons) ((link-set streets directed-streets) with [self != [undesired-street] of current-mover])
        let full-path nw:turtles-on-weighted-path-to [destination-beacon] of myself "weight"

        ;; If there are more than one beacon in the path I haven't reached my destination
        ;; nw-path returns with the current beacon, so we take only the tail of the list
        if not empty? but-first full-path
          [ ask myself [set current-beacon item 1 full-path] ]

        ;; if only one item is present in the weighted path
        ;; a destination-beacon has been reached, here we check with if for
        ;; more security
        if self = [destination-beacon] of myself
          [
            ;; remove the current destination-beacon from this list
            ask current-mover [
              ifelse not empty? destination-list
                [run table:get destination-ordering destination-order]
                [ set destination-beacon min-one-of (beacons with [exit-point? = true]) [distance myself]]
              set color [color] of destination-beacon
          ] ]
      ]
    ]
    face current-beacon
  ]
end

to check-if-involuntary-destination [agent tmp-beacon]
  if member? tmp-beacon [destination-list] of agent [
    let new-destination-list filter [? != tmp-beacon] [destination-list] of agent
    ask agent [set destination-list new-destination-list
               movers-data-collect tmp-beacon]
  ]
end

to update-next-patch
  ask movers with [destination-reached = false] [
    set should-move? false

    ;; get an ordered list of patches where i could move
    let oriented-list oriented-list-of-patches neighbors4 in-cone 2 180 with [wall = false]
    foreach reverse oriented-list [
      if free-mover-patch ? [
        face ?
        set should-move? true
      ]
    ]
  ]
end

;; Returns a list of patches that are ordered by in ascending order
;; wrt the difference between movers heading and the position of
;; the patch, i.e. returns an ordered list of the best patches to
;; visit
to-report oriented-list-of-patches [reachable-patches]
  report sort-on [abs subtract-headings [heading] of myself (towards myself + 180) mod 360] reachable-patches
end

;; Reports whether the patch passed has actually enough free space
to-report free-mover-patch [mover-patch]
  ifelse global-crowd-max-at-patch > [count movers-here] of mover-patch
    [report true]
    [report false]
end

;; ==================================================================
;; This is called when a mover gets blocked, he should in some way
;; backtrack to the previous beacon and try to find a path that
;; does not include the current-beacon
;; ==================================================================
to orient-random-mover [random-mover]
  ask previous-beacon [
    let possible-undesired-street one-of my-streets with [other-end = [current-beacon] of random-mover]
    nw:set-context (beacons) ((link-set streets directed-streets) with [self != possible-undesired-street])
    let full-path nw:turtles-on-weighted-path-to [destination-beacon] of myself "weight"

    if not empty? full-path [
      ask myself [
        ;; if there is another way of reaching the current destination
        ;; then put the current street as undesirable
        set undesired-street possible-undesired-street
        ;; if my previous beacon is different from the current and destination I should move
        ;; towards it => (item 0 full-path), otherwise there will be more than one element
        ;; in the full-path and it is more convenient to move towards the (item 1 full-path)
        ifelse current-beacon != previous-beacon ;and previous-beacon != destination-beacon
          [set current-beacon item 0 full-path]
          [ifelse previous-beacon != destination-beacon
            [set current-beacon item 1 full-path]
            [set current-beacon item 0 full-path]
          ]
      ]
    ]
  ]
end

;; ==================================================================
;; DESTINATION STRATEGIES
;; ==================================================================
;; Called inside the mover context

to set-destination-min-distance
  let origin-beacon current-beacon
  set destination-list sort-by [
    [nw:weighted-distance-to origin-beacon "weight"] of ?1 <
      [nw:weighted-distance-to origin-beacon "weight"] of ?2 ] destination-list
  set destination-beacon item 0 destination-list
end

to set-destination-ordered-list
  set destination-beacon item 0 destination-list
end

;; ==================================================================
;; SOME CONTROL PROCEDURES
;; ==================================================================
to toggle-graph-view
  ask streets [set hidden? not hidden?]
  ask beacons with [interest-point? = false] [set hidden? not hidden?]
end

to change-poi
  toggle-graph-view
  let poi-die one-of beacons with [interest-point? = true]
  ask one-of beacons with [interest-point? = false and entry-point? = false and exit-point? = false] [
    set interest-point? true
  ]
  ask poi-die [set interest-point? false]
  toggle-graph-view
end

;; ==================================================================
;; GLOBAL STATE
;; ==================================================================

to-report get-global-state
  report global-state
end
to update-global-state
  let final-list []
  foreach global-state-grid [
    let grid-patches patches with [pxcor > (item 0 ?) and pxcor < (item 1 ?) and pycor > (item 2 ?) and pycor < (item 3 ?)]
    set final-list lput (sum [count movers-here] of grid-patches) final-list
  ]
  set global-state final-list
end

;; Invoce at the beginning to calculate the grid coordiantes
to setup-global-state-grid
  set global-state-grid (get-grid-map-coordinates global-state-x global-state-y)
end

;; Returns a list of coordinates of a grid division [[x dx y dy] ...]
to-report get-grid-map-coordinates [gx gy]
  let final-list []
  foreach get-grid-map-coordinates-x gx [
    let current-x ?
    foreach get-grid-map-coordinates-y gy [
      set final-list lput (sentence current-x ?) final-list
    ]
  ]
  report final-list
end
to-report get-grid-map-coordinates-x [g]
  report n-values g [(list (? * world-width / g) ((? + 1) * world-width / g))]
end
to-report get-grid-map-coordinates-y [g]
  report n-values g [(list (? * world-height / g) ((? + 1) * world-height / g))]
end

;; ==================================================================
;; RESULTS OUTPUT
;; ==================================================================
;; Called at the end by the BehaviourSpace

to register-results
  ;; open a file
  file-open (word "experiment_" behaviorspace-experiment-name "_run_" behaviorspace-run-number ".csv")

  let onei first table:keys global-movers-results
  file-print to-csv (list "who" (to-csv map [item 0 ?] table:get global-movers-results onei))
  foreach table:keys global-movers-results [
    file-print to-csv (list ? (to-csv map [item 1 ?] table:get global-movers-results ?))
  ]

  ;; close it
  file-close
end

to-report to-csv [l]
  report reduce [(word ?1 ", " ?2)] l
end

;; Populate the table movers-data with some information needed for results
to movers-data-setup
  set movers-data table:make
  table:put movers-data "enter" ticks
  table:put movers-data "total_agents" count movers

  update-global-state
  let idx 0
  foreach get-global-state [
    table:put movers-data (word "gs_" idx) ?
    set idx idx + 1
  ]
end

;; Collect the data before a mover dies, and then put them in global var
to movers-data-collect [passing-beacon]
  ;; Only collect the time the first time we pass a destination
  ifelse table:has-key? movers-data (word "exit_" passing-beacon)
    []
    [table:put movers-data (word "exit_" passing-beacon) ticks]
end

to movers-data-move-to-global
  table:put global-movers-results who table:to-list movers-data
end

;; ==================================================================
;; NEEDED BY JAVANETLOGO GENERATOR
;; ==================================================================

to-report get-interest-beacons [coor-list]
  let list-of-beacons []
  foreach  coor-list [
    set list-of-beacons lput (item 0 sort (beacons-on patch (item 0 ?) (item 1 ?))) list-of-beacons
  ]
  report list-of-beacons
end
;;creates movers for the initial state iterating on the initial-state global variable
;;wich has the following structure: initial-state = [[x y behavior-id #movers behavior-id #movers ...]...]
to set-world-initial-state
  foreach initial-state [
    let local-state []
    set local-state lput ? local-state

    let local-parameters but-first ?
    set local-parameters but-first local-parameters

    ask item 0 get-interest-beacons map [list (world-offset + item 0 ?) (world-offset + item 1 ?)] local-state[
      foreach n-values ( floor (length local-parameters )/ 2) [?] [
        let local-behavior (item (? * 2) local-parameters)
        foreach n-values (item ((? * 2) + 1) local-parameters) [?][
          ask one-of patches in-radius intersection-radius [sprout-movers 1 [
            set current-beacon min-one-of beacons [distance myself]
            set mover-behavior local-behavior
            standard-mover-settings]
          ]
        ]
      ]
    ]
  ]
end

;; Set some defaults and globals for this model
to default-configuration
  set-default-shape beacons "box"
  set-default-shape movers "circle"
  set global-crowd-max-at-patch 5

  ;; List of possible destination ordering strategies
  set destination-ordering table:make
  table:put destination-ordering "minDistance" "set-destination-min-distance"
  table:put destination-ordering "orderedList" "set-destination-ordered-list"

  ;; Results
  set global-movers-results table:make

  ;; Results grid size for result skewness
  set global-state-x 2
  set global-state-y 2
  setup-global-state-grid
  update-global-state
  ;; List of various destinations

  ;; Behaviors get populated in a map and are a list of beacons
  set behaviors-map table:make
  table:put behaviors-map 0 get-interest-beacons map [ list (world-offset + item 0 ?) (world-offset + item 1 ?) ] [ [(0) (20)] ]
  table:put behaviors-map 1 get-interest-beacons map [ list (world-offset + item 0 ?) (world-offset + item 1 ?) ] [ [(20) (0)] ]

  ;;===end

  ;; Beacons that are part of behaviours should be given interest-point? true
  ;; example:
  foreach table:to-list behaviors-map [
    foreach item 1 ? [ask ? [set interest-point? true]]
  ]
  set global-list-interest-points sort beacons with [interest-point? = true]
  set global-list-exit-points sort beacons with [exit-point? = true]

  set initial-state []
  populate-initial-state
  set-world-initial-state
end

to populate-initial-state
  ;; Populate the initial state of movers around the world
  ;; example: [x y behavior_id n_movers]
  set initial-state lput [0 0 0 20 1 20] initial-state
  set initial-state lput [0 20 0 20 1 20] initial-state
  set initial-state lput [20 0 0 20 1 20] initial-state
  set initial-state lput [20 20 0 20 1 20] initial-state

  ;;===end
end
@#$#@#$#@
GRAPHICS-WINDOW
305
10
741
407
-1
-1
6.0
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
70
0
60
0
0
1
ticks
30.0

BUTTON
204
136
282
169
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

INPUTBOX
36
67
281
127
import-filename
netlogo.map
1
0
String

SWITCH
36
223
151
256
use-exits
use-exits
0
1
-1000

SWITCH
166
223
288
256
use-entries
use-entries
0
1
-1000

TEXTBOX
51
193
201
211
Control the movers
12
0.0
1

TEXTBOX
45
33
280
65
Import the world from external file
12
0.0
1

BUTTON
41
274
112
307
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
123
274
288
307
NIL
toggle-graph-view
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
124
321
236
354
NIL
change-poi
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
55
462
227
495
exit-ratio
exit-ratio
0
0.1
0.054
0.001
1
NIL
HORIZONTAL

SLIDER
65
521
237
554
global-patience
global-patience
1
100
10
1
1
NIL
HORIZONTAL

SLIDER
56
421
228
454
entry-ratio
entry-ratio
0
0.100
0.05
0.001
1
NIL
HORIZONTAL

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
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>register-results</final>
    <timeLimit steps="1000"/>
    <enumeratedValueSet variable="import-filename">
      <value value="&quot;grids/grid_4_poi_3_01.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="entry-ratio">
      <value value="0.061"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exit-ratio">
      <value value="0.087"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-patience">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-exits">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-entries">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
