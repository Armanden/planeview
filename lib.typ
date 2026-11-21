#import "@preview/cetz:0.4.2"
#import "@preview/cetz-plot:0.1.3": plot

#let _function-names = (
  "sin", "cos", "tan", "asin", "acos", "atan",
  "arcsin", "arccos", "arctan", "sinh", "cosh", "tanh",
  "asinh", "acosh", "atanh", "sqrt", "exp", "ln", "log",
  "abs", "sign", "floor", "ceil", "round", "sec", "csc",
  "cot", "min", "max", "pow",
)

#let _limit = 1e9

#let _digit(c) = c >= "0" and c <= "9"
#let _alpha(c) = (c >= "a" and c <= "z") or (c >= "A" and c <= "Z")
#let _ident-char(c) = _alpha(c) or _digit(c) or c == "_"
#let _is-function-name(s) = s in _function-names

#let _tokenize(input) = {
  let s = input.replace("−", "-").replace("×", "*").replace("·", "*")
    .replace("÷", "/").replace("π", "pi")
  let chars = s.clusters()
  let tokens = ()
  let i = 0
  while i < chars.len() {
    let c = chars.at(i)
    if c == " " or c == "\t" or c == "\n" {
      i += 1
    } else if c == "+" or c == "-" or c == "*" or c == "/" or c == "^" or c == "=" {
      tokens.push((kind: "op", text: c))
      i += 1
    } else if c == "(" or c == ")" or c == "," {
      tokens.push((kind: "paren", text: c))
      i += 1
    } else if _digit(c) or (c == "." and i + 1 < chars.len() and _digit(chars.at(i + 1))) {
      let j = i
      while j < chars.len() and _digit(chars.at(j)) { j += 1 }
      if j < chars.len() and chars.at(j) == "." {
        j += 1
        while j < chars.len() and _digit(chars.at(j)) { j += 1 }
      }
      if j < chars.len() and (chars.at(j) == "e" or chars.at(j) == "E") {
        let k = j + 1
        if k < chars.len() and (chars.at(k) == "+" or chars.at(k) == "-") { k += 1 }
        if k < chars.len() and _digit(chars.at(k)) {
          j = k
          while j < chars.len() and _digit(chars.at(j)) { j += 1 }
        }
      }
      tokens.push((kind: "num", text: chars.slice(i, j).join()))
      i = j
    } else if _alpha(c) {
      let j = i
      while j < chars.len() and _ident-char(chars.at(j)) { j += 1 }
      let name = chars.slice(i, j).join()
      if _is-function-name(name) {
        tokens.push((kind: "func", text: name))
      } else {
        let rest = name
        while rest.len() > 0 {
          if rest.starts-with("tau") {
            tokens.push((kind: "var", text: "tau"))
            rest = rest.slice(3)
          } else if rest.starts-with("pi") {
            tokens.push((kind: "var", text: "pi"))
            rest = rest.slice(2)
          } else {
            let first = rest.clusters().at(0)
            if first in ("x", "y", "z", "e") {
              tokens.push((kind: "var", text: first))
              rest = rest.slice(1)
            } else {
              tokens.push((kind: "var", text: rest))
              rest = ""
            }
          }
        }
      }
      i = j
    } else {
      panic("Unexpected character in expression: " + c)
    }
  }
  tokens.push((kind: "eof", text: ""))
  tokens
}

#let _tok(tokens, pos, offset: 0) = {
  let i = pos + offset
  if i >= tokens.len() { return tokens.at(tokens.len() - 1) }
  tokens.at(i)
}

#let _is-op(t, op) = t.kind == "op" and t.text == op
#let _is-paren(t, p) = t.kind == "paren" and t.text == p
#let _starts-atom(t) = t.kind in ("num", "var", "func") or _is-paren(t, "(")

#let _num(v) = (kind: "num", value: float(v))
#let _var(n) = (kind: "var", name: n)
#let _bin(op, a, b) = (kind: "binary", op: op, lhs: a, rhs: b)
#let _un(op, a) = (kind: "unary", op: op, value: a)
#let _call(name, args) = (kind: "call", name: name, args: args)

#let _parse-pratt(tokens, pos, min-prec) = {
  let t = _tok(tokens, pos)
  let node
  let p = pos

  if _is-op(t, "+") or _is-op(t, "-") {
    let (v, np) = _parse-pratt(tokens, pos + 1, 30)
    node = _un(t.text, v)
    p = np
  } else if t.kind == "num" {
    node = _num(t.text)
    p = pos + 1
  } else if t.kind == "var" {
    node = _var(t.text)
    p = pos + 1
  } else if _is-paren(t, "(") {
    let (v, np) = _parse-pratt(tokens, pos + 1, 0)
    assert(_is-paren(_tok(tokens, np), ")"), message: "Expected ')'")
    node = v
    p = np + 1
  } else if t.kind == "func" {
    let name = t.text
    p = pos + 1
    let exponent = none
    if _is-op(_tok(tokens, p), "^") {
      let (e, np) = _parse-pratt(tokens, p + 1, 40)
      exponent = e
      p = np
    }
    let args = ()
    if _is-paren(_tok(tokens, p), "(") {
      p += 1
      if not _is-paren(_tok(tokens, p), ")") {
        while true {
          let (a, np) = _parse-pratt(tokens, p, 0)
          args.push(a)
          p = np
          if _is-paren(_tok(tokens, p), ")") { break }
          assert(_is-paren(_tok(tokens, p), ","), message: "Expected ',' or ')' in function arguments")
          p += 1
        }
      }
      assert(_is-paren(_tok(tokens, p), ")"), message: "Expected ')'")
      p += 1
    } else if _starts-atom(_tok(tokens, p)) {
      let (a, np) = _parse-pratt(tokens, p, 30)
      args.push(a)
      p = np
    } else {
      panic("Expected argument for " + name)
    }
    node = _call(name, args)
    if exponent != none { node = _bin("^", node, exponent) }
  } else {
    panic("Unexpected token in expression: " + repr(t))
  }

  while true {
    let op = _tok(tokens, p)
    let prec = 0
    let right-assoc = false
    if (_is-op(op, "+") or _is-op(op, "-")) { prec = 10 }
    else if (_is-op(op, "*") or _is-op(op, "/")) { prec = 20 }
    else if _starts-atom(op) { prec = 20 }
    else if _is-op(op, "^") {
      prec = 40
      right-assoc = true
    }
    if prec == 0 or prec < min-prec { break }
    let op-text = if _starts-atom(op) { "*" } else { op.text }
    let (rhs, np) = _parse-pratt(tokens, p + (if _starts-atom(op) { 0 } else { 1 }), if right-assoc { prec } else { prec + 1 })
    node = _bin(op-text, node, rhs)
    p = np
  }
  (node, p)
}

#let _parse-expression-string(s) = {
  let tokens = _tokenize(s)
  if tokens.len() == 1 { return _num(0) }
  let (node, pos) = _parse-pratt(tokens, 0, 0)
  assert(_tok(tokens, pos).kind == "eof", message: "Unexpected trailing input: " + s)
  node
}

#let _parse-equation(eq) = {
  let parts = eq.split("=")
  assert(parts.len() == 2, message: "Equation must contain exactly one '=': " + eq)
  (_parse-expression-string(parts.at(0)), _parse-expression-string(parts.at(1)))
}

#let _clip(v) = calc.max(-_limit, calc.min(_limit, v))

#let _safe-div(a, b) = {
  if b == 0 {
    if a == 0 { return 0.0 }
    return if a < 0 { -_limit } else { _limit }
  }
  a / b
}

#let _safe-pow(a, b) = {
  if a < 0 and calc.round(b) != b { return -_limit }
  if a == 0 and b < 0 { return _limit }
  calc.pow(a, b)
}

#let _eval(node, env) = {
  let k = node.kind
  if k == "num" { return node.value }
  if k == "var" {
    let n = node.name
    if n == "pi" { return calc.pi }
    if n == "e" { return calc.e }
    if n == "tau" { return calc.tau }
    if n in env { return env.at(n) }
    panic("Unknown variable: " + n)
  }
  if k == "unary" {
    let v = _eval(node.value, env)
    return if node.op == "-" { -v } else { v }
  }
  if k == "binary" {
    let a = _eval(node.lhs, env)
    let b = _eval(node.rhs, env)
    if node.op == "+" { return a + b }
    if node.op == "-" { return a - b }
    if node.op == "*" { return a * b }
    if node.op == "/" { return _safe-div(a, b) }
    if node.op == "^" { return _safe-pow(a, b) }
    panic("Unknown operator: " + node.op)
  }
  if k == "call" {
    let name = node.name
    let args = node.args.map(a => _eval(a, env))
    assert(args.len() >= 1, message: "Function needs an argument: " + name)
    let a = args.at(0)
    if name == "sin" { return calc.sin(a) }
    if name == "cos" { return calc.cos(a) }
    if name == "tan" { return calc.tan(a) }
    if name == "asin" or name == "arcsin" {
      if a < -1 or a > 1 { return -_limit }
      return calc.asin(a) / 1rad
    }
    if name == "acos" or name == "arccos" {
      if a < -1 or a > 1 { return -_limit }
      return calc.acos(a) / 1rad
    }
    if name == "atan" or name == "arctan" { return calc.atan(a) / 1rad }
    if name == "sinh" { return calc.sinh(a) }
    if name == "cosh" { return calc.cosh(a) }
    if name == "tanh" { return calc.tanh(a) }
    if name == "asinh" { return calc.asinh(a) }
    if name == "acosh" {
      if a < 1 { return -_limit }
      return calc.acosh(a)
    }
    if name == "atanh" {
      if calc.abs(a) >= 1 { return -_limit }
      return calc.atanh(a)
    }
    if name == "sqrt" {
      if a < 0 { return -_limit }
      return calc.sqrt(a)
    }
    if name == "exp" { return calc.exp(a) }
    if name == "ln" {
      if a <= 0 { return -_limit }
      return calc.ln(a)
    }
    if name == "log" {
      if a <= 0 { return -_limit }
      if args.len() >= 2 {
        let b = args.at(1)
        if b <= 0 or b == 1 { return -_limit }
        return calc.ln(a) / calc.ln(b)
      }
      return calc.log(a)
    }
    if name == "abs" { return calc.abs(a) }
    if name == "sign" { return if a < 0 { -1.0 } else if a > 0 { 1.0 } else { 0.0 } }
    if name == "floor" { return calc.floor(a) }
    if name == "ceil" { return calc.ceil(a) }
    if name == "round" { return calc.round(a) }
    if name == "sec" { return _safe-div(1, calc.cos(a)) }
    if name == "csc" { return _safe-div(1, calc.sin(a)) }
    if name == "cot" { return _safe-div(1, calc.tan(a)) }
    if name == "min" { return calc.min(..args) }
    if name == "max" { return calc.max(..args) }
    if name == "pow" { assert(args.len() >= 2, message: "pow needs two arguments"); return _safe-pow(a, args.at(1)) }
    panic("Unknown function: " + name)
  }
  panic("Unknown expression node: " + repr(node))
}

#let _is-var(node, name) = node.kind == "var" and node.name == name

#let _vars-used(node) = {
  if node.kind == "var" {
    if node.name in ("x", "y", "z") { return (node.name,) }
    return ()
  }
  if node.kind == "unary" { return _vars-used(node.value) }
  if node.kind == "binary" {
    let result = _vars-used(node.lhs)
    for v in _vars-used(node.rhs) {
      if not v in result { result.push(v) }
    }
    return result
  }
  if node.kind == "call" {
    let result = ()
    for a in node.args {
      for v in _vars-used(a) {
        if not v in result { result.push(v) }
      }
    }
    return result
  }
  ()
}

#let _zero-coeffs() = (x: 0.0, y: 0.0, z: 0.0, k: 0.0)
#let _scale-coeffs(s, c) = (x: s * c.x, y: s * c.y, z: s * c.z, k: s * c.k)
#let _add-coeffs(a, b, s: 1.0) = (x: a.x + s * b.x, y: a.y + s * b.y, z: a.z + s * b.z, k: a.k + s * b.k)
#let _is-constant-coeff(c) = c != none and c.x == 0 and c.y == 0 and c.z == 0

#let _linear-coeffs(node) = {
  if node.kind == "num" { return (x: 0.0, y: 0.0, z: 0.0, k: node.value) }
  if node.kind == "var" {
    if node.name == "x" { return (x: 1.0, y: 0.0, z: 0.0, k: 0.0) }
    if node.name == "y" { return (x: 0.0, y: 1.0, z: 0.0, k: 0.0) }
    if node.name == "z" { return (x: 0.0, y: 0.0, z: 1.0, k: 0.0) }
    if node.name == "pi" { return (x: 0.0, y: 0.0, z: 0.0, k: calc.pi) }
    if node.name == "e" { return (x: 0.0, y: 0.0, z: 0.0, k: calc.e) }
    if node.name == "tau" { return (x: 0.0, y: 0.0, z: 0.0, k: calc.tau) }
    return none
  }
  if node.kind == "unary" {
    let c = _linear-coeffs(node.value)
    if c == none { return none }
    return if node.op == "-" { _scale-coeffs(-1.0, c) } else { c }
  }
  if node.kind == "binary" {
    if node.op == "+" or node.op == "-" {
      let a = _linear-coeffs(node.lhs)
      let b = _linear-coeffs(node.rhs)
      if a == none or b == none { return none }
      return _add-coeffs(a, b, s: if node.op == "-" { -1.0 } else { 1.0 })
    }
    if node.op == "*" {
      let a = _linear-coeffs(node.lhs)
      let b = _linear-coeffs(node.rhs)
      if _is-constant-coeff(b) { return _scale-coeffs(b.k, a) }
      if _is-constant-coeff(a) { return _scale-coeffs(a.k, b) }
      return none
    }
    if node.op == "/" {
      let a = _linear-coeffs(node.lhs)
      let b = _linear-coeffs(node.rhs)
      if _is-constant-coeff(b) and b.k != 0 { return _scale-coeffs(1.0 / b.k, a) }
      return none
    }
    if node.op == "^" and node.rhs.kind == "num" and (node.rhs.value == 0 or node.rhs.value == 1) {
      let a = _linear-coeffs(node.lhs)
      if node.rhs.value == 0 { return (x: 0.0, y: 0.0, z: 0.0, k: 1.0) }
      return a
    }
    return none
  }
  none
}

/// Parse a linear equation string like "3x+2z=18" or "y=0" into (cx,cy,cz,d) with cx*x+cy*y+cz*z = d.
#let parse-linear(eq) = {
  let (lhs, rhs) = _parse-equation(eq)
  let c = _linear-coeffs(_bin("-", lhs, rhs))
  assert(c != none, message: "Equation is not linear: " + eq)
  (cx: c.x, cy: c.y, cz: c.z, d: -c.k)
}

/// Plot one or more 2D equations. Linear equations are drawn exactly; other equations (including y=f(x), x=f(y) and implicit curves such as y^2=x^2+sin(xy)) are sampled.
#let plane2d(..args, x-min: -10, x-max: 10, y-min: -10, y-max: 10, size: (8, 6), grid: true, samples: 60) = {
  let equations = args.pos()
  if equations.len() == 1 and type(equations.at(0)) == array { equations = equations.at(0) }
  cetz.canvas(length: 1cm, {
    import cetz.draw: *
    plot.plot(size: size, x-min: x-min, x-max: x-max, y-min: y-min, y-max: y-max, x-grid: grid, y-grid: grid, {
      for eq in equations {
        let (lhs, rhs) = _parse-equation(eq)
        let diff = _bin("-", lhs, rhs)
        let lin = _linear-coeffs(diff)
        if lin != none {
          let a = lin.x
          let b = lin.y
          let c = lin.k
          if calc.abs(b) > 1e-9 {
            plot.add(domain: (x-min, x-max), samples: 2, x => -(a * x + c) / b, label: eq)
          } else if calc.abs(a) > 1e-9 {
            let x0 = -c / a
            plot.add(((x0, y-min), (x0, y-max)), label: eq)
          }
        } else if _is-var(lhs, "y") and "y" not in _vars-used(rhs) and "z" not in _vars-used(rhs) {
          plot.add(domain: (x-min, x-max), samples: samples, x => {
            let v = _eval(rhs, (x: x, y: 0))
            calc.max(y-min - 1, calc.min(y-max + 1, v))
          }, label: eq)
        } else if _is-var(rhs, "y") and "y" not in _vars-used(lhs) and "z" not in _vars-used(lhs) {
          plot.add(domain: (y-min, y-max), samples: samples, t => {
            let v = _eval(lhs, (x: t, y: 0))
            (calc.max(x-min - 1, calc.min(x-max + 1, v)), t)
          }, label: eq)
        } else {
          plot.add-contour(
            (x, y) => _clip(_eval(diff, (x: x, y: y))),
            x-domain: (x-min, x-max),
            y-domain: (y-min, y-max),
            x-samples: samples,
            y-samples: samples,
            z: (0,),
            label: eq,
          )
        }
      }
    })
  })
}

/// Draw a plane ax+by+cz=d in 3D as a quad over given bounds.
#let _draw-plane-3d(coeffs, x-min: -5, x-max: 5, y-min: -5, y-max: 5, z-min: -5, z-max: 5, color: blue) = {
  import cetz.draw: *
  let a = coeffs.cx
  let b = coeffs.cy
  let c0 = coeffs.cz
  let d = coeffs.d
  let pts = ()
  if calc.abs(c0) >= calc.abs(a) and calc.abs(c0) >= calc.abs(b) and calc.abs(c0) > 1e-9 {
    let zf(x, y) = (d - a * x - b * y) / c0
    pts = ((x-min, y-min, zf(x-min, y-min)), (x-max, y-min, zf(x-max, y-min)), (x-max, y-max, zf(x-max, y-max)), (x-min, y-max, zf(x-min, y-max)))
  } else if calc.abs(b) >= calc.abs(a) and calc.abs(b) > 1e-9 {
    let yf(x, z) = (d - a * x - c0 * z) / b
    pts = ((x-min, yf(x-min, z-min), z-min), (x-max, yf(x-max, z-min), z-min), (x-max, yf(x-max, z-max), z-max), (x-min, yf(x-min, z-max), z-max))
  } else if calc.abs(a) > 1e-9 {
    let xf(y, z) = (d - b * y - c0 * z) / a
    pts = ((xf(y-min, z-min), y-min, z-min), (xf(y-max, z-min), y-max, z-min), (xf(y-max, z-max), y-max, z-max), (xf(y-min, z-max), y-min, z-max))
  } else {
    return
  }
  line(..pts, close: true, stroke: (paint: color, thickness: 0.8pt))
}

#let _draw-surface-3d(expr, x-min: -5, x-max: 5, y-min: -5, y-max: 5, z-min: -5, z-max: 5, samples: 16, color: blue) = {
  import cetz.draw: *
  let z-value(x, y) = calc.max(z-min - 0.1, calc.min(z-max + 0.1, _eval(expr, (x: x, y: y))))
  for i in range(0, samples + 1) {
    let x = x-min + i / samples * (x-max - x-min)
    let pts = ()
    for j in range(0, samples + 1) {
      let y = y-min + j / samples * (y-max - y-min)
      pts.push((x, y, z-value(x, y)))
    }
    line(..pts, stroke: (paint: color, thickness: 0.4pt))
  }
  for j in range(0, samples + 1) {
    let y = y-min + j / samples * (y-max - y-min)
    let pts = ()
    for i in range(0, samples + 1) {
      let x = x-min + i / samples * (x-max - x-min)
      pts.push((x, y, z-value(x, y)))
    }
    line(..pts, stroke: (paint: color, thickness: 0.4pt))
  }
}

/// Plot 3D linear equations/planes, or explicit surfaces z=f(x,y).
#let plane3d(..args, x-min: -6, x-max: 6, y-min: -6, y-max: 6, z-min: -6, z-max: 6, rot-x: 70deg, rot-z: 45deg, surface-samples: 16) = {
  let equations = args.pos()
  if equations.len() == 1 and type(equations.at(0)) == array { equations = equations.at(0) }
  let colors = (blue, red, green, orange, purple)
  cetz.canvas(length: 1cm, {
    import cetz.draw: *
    ortho(x: -rot-x, y: 0deg, z: rot-z, {
      line((x-min, 0, 0), (x-max, 0, 0), stroke: gray)
      content((x-max + 0.4, 0, 0), [$x$])
      line((0, y-min, 0), (0, y-max, 0), stroke: gray)
      content((0, y-max + 0.4, 0), [$y$])
      line((0, 0, z-min), (0, 0, z-max), stroke: gray)
      content((0, 0, z-max + 0.4), [$z$])

      for (i, eq) in equations.enumerate() {
        let (lhs, rhs) = _parse-equation(eq)
        let diff = _bin("-", lhs, rhs)
        let lin = _linear-coeffs(diff)
        let col = colors.at(calc.rem(i, colors.len()))
        if lin != none {
          _draw-plane-3d((cx: lin.x, cy: lin.y, cz: lin.z, d: -lin.k), x-min: x-min, x-max: x-max, y-min: y-min, y-max: y-max, z-min: z-min, z-max: z-max, color: col)
        } else if _is-var(lhs, "z") and "z" not in _vars-used(rhs) {
          _draw-surface-3d(rhs, x-min: x-min, x-max: x-max, y-min: y-min, y-max: y-max, z-min: z-min, z-max: z-max, samples: surface-samples, color: col)
        } else if _is-var(rhs, "z") and "z" not in _vars-used(lhs) {
          _draw-surface-3d(lhs, x-min: x-min, x-max: x-max, y-min: y-min, y-max: y-max, z-min: z-min, z-max: z-max, samples: surface-samples, color: col)
        } else {
          panic("Unsupported 3D equation: " + eq)
        }
      }
    })
  })
}

/// Convenience: auto-detect 2D vs 3D from presence of `z` and plot.
#let show-eq(..args) = {
  let equations = args.pos()
  if equations.len() == 1 and type(equations.at(0)) == array { equations = equations.at(0) }
  let is3d = equations.any(e => "z" in e)
  if is3d { plane3d(..equations, ..args.named()) } else { plane2d(..equations, ..args.named()) }
}

/// Alias matching the requested API, e.g. `#ex("3x+2z=18", "y=0")`
#let ex = show-eq
