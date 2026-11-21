#import "lib.typ": ex, plane2d

#set page(width: auto, height: auto, margin: 1cm)

= 2D example
#plane2d("y=2x+1", "x+y=4", x-min: -5, x-max: 5, y-min: -5, y-max: 5)

= 3D example
#ex("3x+2z=18", "y=0")

= functions
#plane2d("y=sin(x)", "y=sqrt(x)", "y=e^x", x-min: -5, x-max: 5, y-min: -5, y-max: 5)

= implicit curve
#plane2d("y^2 = x^2 + sin(xy)", x-min: -10, x-max: 10, y-min: -10, y-max: 10)
