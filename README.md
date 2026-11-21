# planeview

A small Typst extension/package built on [CeTZ](https://github.com/cetz-package/cetz) to plot 2D equations and 3D planes/surfaces from strings without manually computing sample points.

## Usage

```typst
#import "lib.typ": ex, show-eq, plane2d, plane3d

// Auto-detect 2D vs 3D (as requested)
#ex("3x+2z=18", "y=0")

// 2D lines, functions and implicit curves
#plane2d("y=2x+1", "x+y=4", x-min: -5, x-max: 5, y-min: -5, y-max: 5)
#plane2d("y=sin(x)", "y=sqrt(x)", "y=e^x", x-min: -5, x-max: 5)
#plane2d("y^2 = x^2 + sin(xy)", x-min: -10, x-max: 10, y-min: -10, y-max: 10)

// 3D planes and explicit surfaces
#plane3d("3x+2z=18", "y=0", x-min: -6, x-max: 6, y-min: -6, y-max: 6, z-min: -6, z-max: 6)
#ex("z=sin(x)cos(y)")
```

## Supported input

- Linear equations in `x`, `y`, `z` with `=` (e.g. `3x+2z=18`, `y=0`, `x+y=4`, `y=2x+1`).
- Explicit functions such as `y=exp(x)`, `y=sqrt(x)`, `y=sin(x)cos(x)`, `x=cos(y)` and `z=sin(x)cos(y)`.
- Implicit 2D curves such as `x^2+y^2=25` and `y^2=x^2+sin(xy)`, rendered with marching squares.
- Operators `+ - * / ^`, implicit multiplication (`2x`, `xy`, `2(x+1)`), parentheses, decimals and scientific notation.
- Constants `pi`, `e`, `tau` and functions `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `sqrt`, `exp`, `ln`, `log`, `abs`, `sec`, `csc`, `cot`, `min`, `max`, `pow` and others. Powers of functions such as `sin^2(x)` are supported.
- Equations containing `z` are rendered as 3D planes (linear) or wireframe surfaces (`z=f(x,y)`); otherwise as 2D lines/curves.

## Notes

- Linear 2D equations are drawn exactly; nonlinear 2D equations are sampled. Increase `samples` (default `60`) for smoother curves.
- 3D surfaces use `surface-samples` (default `16`) and are drawn as a wireframe.
- Uses `@preview/cetz:0.4.2` and `@preview/cetz-plot:0.1.3`.
