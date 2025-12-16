(define-module (hnh util color parse)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :use-module (ice-9 peg)
  :use-module (ice-9 curried-definitions)
  :use-module (srfi srfi-88)
  :use-module (hnh util color)
  :export (parse-color
           parse-hex-rgb)
  )

;; https://www.w3.org/TR/css-color-3/

;; note. if rgba values are not supported by a user agent, they should be treated like unrecognized values per the css forward compatibility parsing rules ([css21], chapter 4). rgba values must not be treated as simply an rgb value with the opacity ignored. 

(define (clamp v min max)
  (cond ((< v min) min)
        ((> v max) max)
        (else v)))



;;; Firefox (2025-12-03) doesn't support spaces between the negation
;;; prefix and the number, or between the number and the percentage sign.

(define-peg-pattern integer all
  (and (? (or "+" "-")) (+ (range #\0 #\9))))

(define-peg-pattern float all
  (and (? integer) "." (? integer)))

(define-peg-pattern percentage all
  (and float (ignore "%")))

(define-peg-pattern number body
  (or percentage float integer))

(define-peg-pattern ws none
  (* (or " " "	" "\n")))

(define-peg-pattern word body
  (or "rgba"
      "rgb"
      "hsla"
      "hsl"))

(define-peg-pattern function-like all
  (and word (ignore "(")
       ws number ws (ignore ",")
       ws number ws (ignore ",")
       ws number ws
       (? (and (ignore ",") ws number ws))
       (ignore ")")))



(define short-color-rx
  (make-regexp "^#([0-9a-f])([0-9a-f])([0-9a-f])([0-9a-f])?$" regexp/icase))

(define full-color-rx
  (make-regexp "^#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})?$" regexp/icase))

(define (parsed-number->number expr)
  (match expr
    (`(integer ,x)
     (string->number x 10))
    (`(percentage (integer ,x))
     (/ (string->number x 10) 100))

    (`(float "." (integer ,x))
     (string->number (string-append "." x)))
    (`(float (integer ,x) ".")
     (string->number x))
    (`(float (integer ,x) "." (integer ,y))
     (string->number (string-append x "." y)))

    (`(percentage (float "." (integer ,x)))
     (/ (string->number (string-append "." x)) 100))
    (`(percentage (float (integer ,x) "."))
     (/ (string->number x) 100))
    (`(percentage (float (integer ,x) "." (integer ,y)))
     (/ (string->number (string-append x "." y)) 100))
    ))

(define* ((rgba-match-color optional: (converter identity)) m)
  (define (conv v) (clamp (string->number (converter v) 16) 0 255))
  (color-rgba
   r: (conv (match:substring m 1))
   g: (conv (match:substring m 2))
   b: (conv (match:substring m 3))
   a: (cond ((match:substring m 4)
             => (lambda (s) (/ (conv s) 255)))
            (else 1))))




(define color-names
  (map (lambda (p)
         (cons (car p)
               (cond ((regexp-exec full-color-rx (cdr p))
                      => (rgba-match-color))
                     (else (scm-error 'misc-error "color-names"
                                      "Pre-configured color ~s in improper format: ~s"
                                      (list (car p) (cdr p)) #f)))))
   '(("transparent" . "#00000000")
     ("aliceblue" . "#f0f8ff")
     ("antiquewhite" . "#faebd7")
     ("aqua" . "#00ffff")
     ("aquamarine" . "#7fffd4")
     ("azure" . "#f0ffff")
     ("beige" . "#f5f5dc")
     ("bisque" . "#ffe4c4")
     ("black" . "#000000")
     ("blanchedalmond" . "#ffebcd")
     ("blue" . "#0000ff")
     ("blueviolet" . "#8a2be2")
     ("brown" . "#a52a2a")
     ("burlywood" . "#deb887")
     ("cadetblue" . "#5f9ea0")
     ("chartreuse" . "#7fff00")
     ("chocolate" . "#d2691e")
     ("coral" . "#ff7f50")
     ("cornflowerblue" . "#6495ed")
     ("cornsilk" . "#fff8dc")
     ("crimson" . "#dc143c")
     ("cyan" . "#00ffff")
     ("darkblue" . "#00008b")
     ("darkcyan" . "#008b8b")
     ("darkgoldenrod" . "#b8860b")
     ("darkgray" . "#a9a9a9")
     ("darkgreen" . "#006400")
     ("darkgrey" . "#a9a9a9")
     ("darkkhaki" . "#bdb76b")
     ("darkmagenta" . "#8b008b")
     ("darkolivegreen" . "#556b2f")
     ("darkorange" . "#ff8c00")
     ("darkorchid" . "#9932cc")
     ("darkred" . "#8b0000")
     ("darksalmon" . "#e9967a")
     ("darkseagreen" . "#8fbc8f")
     ("darkslateblue" . "#483d8b")
     ("darkslategray" . "#2f4f4f")
     ("darkslategrey" . "#2f4f4f")
     ("darkturquoise" . "#00ced1")
     ("darkviolet" . "#9400d3")
     ("deeppink" . "#ff1493")
     ("deepskyblue" . "#00bfff")
     ("dimgray" . "#696969")
     ("dimgrey" . "#696969")
     ("dodgerblue" . "#1e90ff")
     ("firebrick" . "#b22222")
     ("floralwhite" . "#fffaf0")
     ("forestgreen" . "#228b22")
     ("fuchsia" . "#ff00ff")
     ("gainsboro" . "#dcdcdc")
     ("ghostwhite" . "#f8f8ff")
     ("gold" . "#ffd700")
     ("goldenrod" . "#daa520")
     ("gray" . "#808080")
     ("green" . "#008000")
     ("greenyellow" . "#adff2f")
     ("grey" . "#808080")
     ("honeydew" . "#f0fff0")
     ("hotpink" . "#ff69b4")
     ("indianred" . "#cd5c5c")
     ("indigo" . "#4b0082")
     ("ivory" . "#fffff0")
     ("khaki" . "#f0e68c")
     ("lavender" . "#e6e6fa")
     ("lavenderblush" . "#fff0f5")
     ("lawngreen" . "#7cfc00")
     ("lemonchiffon" . "#fffacd")
     ("lightblue" . "#add8e6")
     ("lightcoral" . "#f08080")
     ("lightcyan" . "#e0ffff")
     ("lightgoldenrodyellow" . "#fafad2")
     ("lightgray" . "#d3d3d3")
     ("lightgreen" . "#90ee90")
     ("lightgrey" . "#d3d3d3")
     ("lightpink" . "#ffb6c1")
     ("lightsalmon" . "#ffa07a")
     ("lightseagreen" . "#20b2aa")
     ("lightskyblue" . "#87cefa")
     ("lightslategray" . "#778899")
     ("lightslategrey" . "#778899")
     ("lightsteelblue" . "#b0c4de")
     ("lightyellow" . "#ffffe0")
     ("lime" . "#00ff00")
     ("limegreen" . "#32cd32")
     ("linen" . "#faf0e6")
     ("magenta" . "#ff00ff")
     ("maroon" . "#800000")
     ("mediumaquamarine" . "#66cdaa")
     ("mediumblue" . "#0000cd")
     ("mediumorchid" . "#ba55d3")
     ("mediumpurple" . "#9370db")
     ("mediumseagreen" . "#3cb371")
     ("mediumslateblue" . "#7b68ee")
     ("mediumspringgreen" . "#00fa9a")
     ("mediumturquoise" . "#48d1cc")
     ("mediumvioletred" . "#c71585")
     ("midnightblue" . "#191970")
     ("mintcream" . "#f5fffa")
     ("mistyrose" . "#ffe4e1")
     ("moccasin" . "#ffe4b5")
     ("navajowhite" . "#ffdead")
     ("navy" . "#000080")
     ("oldlace" . "#fdf5e6")
     ("olive" . "#808000")
     ("olivedrab" . "#6b8e23")
     ("orange" . "#ffa500")
     ("orangered" . "#ff4500")
     ("orchid" . "#da70d6")
     ("palegoldenrod" . "#eee8aa")
     ("palegreen" . "#98fb98")
     ("paleturquoise" . "#afeeee")
     ("palevioletred" . "#db7093")
     ("papayawhip" . "#ffefd5")
     ("peachpuff" . "#ffdab9")
     ("peru" . "#cd853f")
     ("pink" . "#ffc0cb")
     ("plum" . "#dda0dd")
     ("powderblue" . "#b0e0e6")
     ("purple" . "#800080")
     ("red" . "#ff0000")
     ("rosybrown" . "#bc8f8f")
     ("royalblue" . "#4169e1")
     ("saddlebrown" . "#8b4513")
     ("salmon" . "#fa8072")
     ("sandybrown" . "#f4a460")
     ("seagreen" . "#2e8b57")
     ("seashell" . "#fff5ee")
     ("sienna" . "#a0522d")
     ("silver" . "#c0c0c0")
     ("skyblue" . "#87ceeb")
     ("slateblue" . "#6a5acd")
     ("slategray" . "#708090")
     ("slategrey" . "#708090")
     ("snow" . "#fffafa")
     ("springgreen" . "#00ff7f")
     ("steelblue" . "#4682b4")
     ("tan" . "#d2b48c")
     ("teal" . "#008080")
     ("thistle" . "#d8bfd8")
     ("tomato" . "#ff6347")
     ("turquoise" . "#40e0d0")
     ("violet" . "#ee82ee")
     ("wheat" . "#f5deb3")
     ("white" . "#ffffff")
     ("whitesmoke" . "#f5f5f5")
     ("yellow" . "#ffff00")
     ("yellowgreen" . "#9acd32"))))




(define (parse-hex-rgb str)
  (cond ((regexp-exec full-color-rx str)
         => (rgba-match-color))
        (else #f)))

;;; This allows "incorrect" number formats for a bunch of
;;; fields. Instead of leading to syntax errors, you'll just get a number
;;; in a really weird range.
(define (parse-color str)
  (cond
   ((assoc-ref color-names str) => identity)

   ((regexp-exec short-color-rx str)
    => (rgba-match-color (lambda (v) (string-append v v))))

   ((parse-hex-rgb str) => identity)

   (else
    (match (peg:tree (match-pattern function-like str))
      (`(function-like "rgb" ,r ,g ,b)
       (color-rgba
        r: (clamp (parsed-number->number r) 0 255)
        g: (clamp (parsed-number->number g) 0 255)
        b: (clamp (parsed-number->number b) 0 255)
        a: 1))
      (`(function-like "rgba" ,r ,g ,b ,a)
       (color-rgba
        r: (clamp (parsed-number->number r) 0 255)
        g: (clamp (parsed-number->number g) 0 255)
        b: (clamp (parsed-number->number b) 0 255)
        a: (clamp (parsed-number->number a) 0 1)))
      (`(function-like "hsl" ,h ,s ,l)
       (color-hsla
        h: (modulo (parsed-number->number h) 360)
        s: (clamp (parsed-number->number s) 0 1)
        l: (clamp (parsed-number->number l) 0 1)
        a: 1))
      (`(function-like "hsla" ,h ,s ,l ,a)
       (color-hsla
        h: (modulo (parsed-number->number h) 360)
        s: (clamp (parsed-number->number s) 0 1)
        l: (clamp (parsed-number->number l) 0 1)
        a: (clamp (parsed-number->number a) 0 1)))))))

