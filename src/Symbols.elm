module Symbols exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Svg exposing (path, svg, text, title)
import Svg.Attributes exposing (d, height, id, viewBox, width)


makePath : String -> String -> Html msg
makePath ariaTitle pathData =
    svg [ width "", height "1.5rem", viewBox "0 0 640 640", attribute "role" "img", attribute "aria-labelledby" "title" ]
        [ title [ id "title" ] [ text ariaTitle ]
        , path [ d pathData ] []
        ]


editOutline : Html msg
editOutline =
    makePath "Edit" "M122 379.1C112 389 104 403 100 417.8L64 545.6C62 553 64 562 71 569C77 575 86 577 94 575.2L222 539.7C236 535 250 527 261 517.1L555 223.1C568 209 576 191 576 172C576 152 568 134 554 120.9L519 85.2C505 71 487 64 468 64C448 64 430 71 416 85.2L122 379.2zM468 112C474 112 480 114 485 119.1L520 154.8C525 159 528 165 528 172C528 178 525 184 520 189.2L468 242.1L397 172L450 119.1C455 114 461 112 468 112zM173 396L364 205.9L434 276L244 466.1L173 396zM145 435.3L204 494.7L122 517.5L145 435.3z"


deleteOutline : Html msg
deleteOutline =
    makePath "Delete" "M232 68.8C236 56 248 48 262 48L377 48C391 48 403 56 407 68.8L424 112L520 112C533 112 544 122 544 136C544 149 533 160 520 160L120 160C106 160 96 149 96 136C96 122 106 112 120 112L216 112L232 68.8zM147 516.8L124 208L172 208L195 513.2C196 521 203 528 211 528L428 528C437 528 443 521 444 513.2L467 208L515 208L492 516.7C489 550 462 576 428 576L211 576C178 576 150 550 147 516.7z"
