
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggpathdiagramr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggpathdiagramr)](https://CRAN.R-project.org/package=ggpathdiagramr)
<!-- badges: end -->

This package is in a very preliminary state. It has some structural
beams and a bit of plumbing but is not yet capable of fulfilling its
eventual purpose: To construct path diagrams in R via ggplot2, but with
a TiKz-like workflow.

## Installation

You can install the development version of ggpathdiagramr like so:

``` r
remotes::install_github("wjschne/ggpathdiagramr")
```

# Angles

Angles have different kinds of units associated with them: degrees,
radians, gradians, and turns.

I like $\pi$ just fine, but I agree with Michael Hartl’s [Tau
Manifesto](https://tauday.com/tau-manifesto) that we would have been off
if we had recognized that the number of radians to complete a full
circle is more fundamental than the number of radians to complete a half
circle. The symbol for this value is $\tau = 2\pi\approx 6.283185$

The `angle` class can take turns (i.e., 1 turn = one full rotation a
circle), degrees (1 turn = 360 degrees), radians (1 turn = $2\pi$ =
$\tau$), or gradians (1 turn = 400 gradians, gons, or grads).

``` r
library(ggpathdiagramr)
# Three equivalent ways of creating the same angle
tau <- 2 * pi

angle(turn = .25)
#> <angle>
#>  @ turn   : num 0.25
#>  @ radian : num 1.57
#>  @ degree : num 90
#>  @ gradian: num 100
```

``` r
angle(radian = 0.25 * tau)
#> <angle>
#>  @ turn   : num 0.25
#>  @ radian : num 1.57
#>  @ degree : num 90
#>  @ gradian: num 100
```

``` r
angle(degree = 180)
#> <angle>
#>  @ turn   : num 0.5
#>  @ radian : num 3.14
#>  @ degree : num 180
#>  @ gradian: num 200
```

``` r
angle(gradian = 200)
#> <angle>
#>  @ turn   : num 0.5
#>  @ radian : num 3.14
#>  @ degree : num 180
#>  @ gradian: num 200
```

Angles can take the three standard trigonometric functions

``` r
theta <- angle(degree = 60)
cos(theta)
#> [1] 0.5
```

``` r
sin(theta)
#> [1] 0.8660254
```

``` r
tan(theta)
#> [1] 1.732051
```

Angles can be added, subtracted, multiplied, and divided. The result is
never negative and always less than 1 turn (360 degrees or $\tau$
radians).

``` r
angle(degree = 30) + angle(degree = 60)
#> <angle>
#>  @ turn   : num 0.25
#>  @ radian : num 1.57
#>  @ degree : num 90
#>  @ gradian: num 100
```

``` r
angle(degree = 330) + angle(degree = 60)
#> <angle>
#>  @ turn   : num 0.0833
#>  @ radian : num 0.524
#>  @ degree : num 30
#>  @ gradian: num 33.3
```

``` r
angle(degree = 10) - angle(degree = 20)
#> <angle>
#>  @ turn   : num 0.972
#>  @ radian : num 6.11
#>  @ degree : num 350
#>  @ gradian: num 389
```

``` r

angle(degree = 20) * 2
#> <angle>
#>  @ turn   : num 0.111
#>  @ radian : num 0.698
#>  @ degree : num 40
#>  @ gradian: num 44.4
```

``` r
angle(degree = 180) * 2
#> <angle>
#>  @ turn   : num 0
#>  @ radian : num 0
#>  @ degree : num 0
#>  @ gradian: num 0
```

### Points

Points have x and y coordinates. They can be added and subtracted

``` r
p1 <- point(1,0)
p2 <- point(0,1)
p3 <- p1 + p2
p3
#> <point>
#>  @ x       : num 1
#>  @ y       : num 1
#>  @ slope   : num 1
#>  @ angle   : <angle>
#>  .. @ turn   : num 0.125
#>  .. @ radian : num 0.785
#>  .. @ degree : num 45
#>  .. @ gradian: num 50
#>  @ distance: num 1.41
#>  @ xy      : num [1, 1:2] 1 1
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : NULL
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r
p3 - p2
#> <point>
#>  @ x       : num 1
#>  @ y       : num 0
#>  @ slope   : num 0
#>  @ angle   : <angle>
#>  .. @ turn   : num 0
#>  .. @ radian : num 0
#>  .. @ degree : num 0
#>  .. @ gradian: num 0
#>  @ distance: num 1
#>  @ xy      : num [1, 1:2] 1 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : NULL
#>  ..  ..$ : chr [1:2] "x" "y"
```

Technically, a point is not a geometric vector, but the `point` object
contains information about the vector that starts in the origin and ends
at the point:

- `@distance`: The distance from the origin to the point (i.e., the
  vector’s magnitude)
- `@slope`: The slope of the line containing the vector
- `@angle`: The angle (in radians) from the line on the x-axis to the
  line containing the vector.

``` r
p3
#> <point>
#>  @ x       : num 1
#>  @ y       : num 1
#>  @ slope   : num 1
#>  @ angle   : <angle>
#>  .. @ turn   : num 0.125
#>  .. @ radian : num 0.785
#>  .. @ degree : num 45
#>  .. @ gradian: num 50
#>  @ distance: num 1.41
#>  @ xy      : num [1, 1:2] 1 1
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : NULL
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r
p3@distance
#> [1] 1.414214
```

``` r
p3@slope
#> [1] 1
```

``` r
p3@angle
#> <angle>
#>  @ turn   : num 0.125
#>  @ radian : num 0.785
#>  @ degree : num 45
#>  @ gradian: num 50
```

A point can be created with radial coordinates

``` r
point(distance = 5, angle = angle(degree = 60))
#> <point>
#>  @ x       : num 2.5
#>  @ y       : num 4.33
#>  @ slope   : num 1.73
#>  @ angle   : <angle>
#>  .. @ turn   : num 0.167
#>  .. @ radian : num 1.05
#>  .. @ degree : num 60
#>  .. @ gradian: num 66.7
#>  @ distance: num 5
#>  @ xy      : num [1, 1:2] 2.5 4.33
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : NULL
#>  ..  ..$ : chr [1:2] "x" "y"
```

If the angle is numeric instead of an angle, it is assumed to be in
radians. The advantage of using the angle function is that under the
hood, the `sinpi`, `cospi`, and `tanpi` functions are used to get the
rounding right at key locations. For example, compare the y coordinate
at $pi$ radians vs. 180 degrees:

``` r
point(distance = 1, angle = pi)@y
#> [1] 1.224606e-16
```

``` r
point(distance = 1, angle = angle(degree = 180))@y
#> [1] 0
```

A point can be converted to a 1 × 2 matrix:

``` r
p3@xy
#>      x y
#> [1,] 1 1
```

``` r
as.matrix(p3)
#>      x y
#> [1,] 1 1
```

A list of points can be created like so:

``` r
pp <- point(x = c(2,4,5), y = c(5,3,2))
pp
#> <point_list> List of 3
#>  $ : <point>
#>   ..@ x       : num 2
#>   ..@ y       : num 5
#>   ..@ slope   : num 2.5
#>   ..@ angle   : <angle>
#>  .. .. @ turn   : num 0.189
#>  .. .. @ radian : num 1.19
#>  .. .. @ degree : num 68.2
#>  .. .. @ gradian: num 75.8
#>   ..@ distance: num 5.39
#>   ..@ xy      : num [1, 1:2] 2 5
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  $ : <point>
#>   ..@ x       : num 4
#>   ..@ y       : num 3
#>   ..@ slope   : num 0.75
#>   ..@ angle   : <angle>
#>  .. .. @ turn   : num 0.102
#>  .. .. @ radian : num 0.644
#>  .. .. @ degree : num 36.9
#>  .. .. @ gradian: num 41
#>   ..@ distance: num 5
#>   ..@ xy      : num [1, 1:2] 4 3
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  $ : <point>
#>   ..@ x       : num 5
#>   ..@ y       : num 2
#>   ..@ slope   : num 0.4
#>   ..@ angle   : <angle>
#>  .. .. @ turn   : num 0.0606
#>  .. .. @ radian : num 0.381
#>  .. .. @ degree : num 21.8
#>  .. .. @ gradian: num 24.2
#>   ..@ distance: num 5.39
#>   ..@ xy      : num [1, 1:2] 5 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ x       : num [1:3] 2 4 5
#>  @ y       : num [1:3] 5 3 2
#>  @ xy      : num [1:3, 1:2] 2 4 5 5 3 2
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : NULL
#>  ..  ..$ : chr [1:2] "x" "y"
#>  @ slope   : num [1:3] 2.5 0.75 0.4
#>  @ angle   : <angle_list> List of 3
#>  .. $ : <angle>
#>  ..  ..@ turn   : num 0.189
#>  ..  ..@ radian : num 1.19
#>  ..  ..@ degree : num 68.2
#>  ..  ..@ gradian: num 75.8
#>  .. $ : <angle>
#>  ..  ..@ turn   : num 0.102
#>  ..  ..@ radian : num 0.644
#>  ..  ..@ degree : num 36.9
#>  ..  ..@ gradian: num 41
#>  .. $ : <angle>
#>  ..  ..@ turn   : num 0.0606
#>  ..  ..@ radian : num 0.381
#>  ..  ..@ degree : num 21.8
#>  ..  ..@ gradian: num 24.2
#>  .. @ turn   : num [1:3] 0.1894 0.1024 0.0606
#>  .. @ radian : num [1:3] 1.19 0.644 0.381
#>  .. @ degree : num [1:3] 68.2 36.9 21.8
#>  .. @ gradian: num [1:3] 75.8 41 24.2
#>  @ distance: num [1:3] 5.39 5 5.39
```

A point list’s properties can be extracted:

``` r
as.matrix(pp)
#>      x y
#> [1,] 2 5
#> [2,] 4 3
#> [3,] 5 2
```

``` r
pp@xy
#>      x y
#> [1,] 2 5
#> [2,] 4 3
#> [3,] 5 2
```

``` r
pp@x
#> [1] 2 4 5
```

``` r
pp@y
#> [1] 5 3 2
```

``` r
pp@slope
#> [1] 2.50 0.75 0.40
```

``` r
pp@distance
#> [1] 5.385165 5.000000 5.385165
```

``` r
pp@angle
#> <angle_list> List of 3
#>  $ : <angle>
#>   ..@ turn   : num 0.189
#>   ..@ radian : num 1.19
#>   ..@ degree : num 68.2
#>   ..@ gradian: num 75.8
#>  $ : <angle>
#>   ..@ turn   : num 0.102
#>   ..@ radian : num 0.644
#>   ..@ degree : num 36.9
#>   ..@ gradian: num 41
#>  $ : <angle>
#>   ..@ turn   : num 0.0606
#>   ..@ radian : num 0.381
#>   ..@ degree : num 21.8
#>   ..@ gradian: num 24.2
#>  @ turn   : num [1:3] 0.1894 0.1024 0.0606
#>  @ radian : num [1:3] 1.19 0.644 0.381
#>  @ degree : num [1:3] 68.2 36.9 21.8
#>  @ gradian: num [1:3] 75.8 41 24.2
```

``` r
pp@angle@degree
#> [1] 68.19859 36.86990 21.80141
```

``` r
pp@angle@radian
#> [1] 1.1902899 0.6435011 0.3805064
```

### Lines

Aside from a vertical line, lines can be constructed from a slope and an
intercept

``` r
l1 <- line(slope = 1, intercept = 3)
l1
#> <line>
#>  @ a          : num -1
#>  @ b          : num 1
#>  @ c          : num -3
#>  @ slope      : num 1
#>  @ intercept  : num 3
#>  @ x_intercept: num -3
#>  @ angle      : <angle>
#>  .. @ turn   : num 0.125
#>  .. @ radian : num 0.785
#>  .. @ degree : num 45
#>  .. @ gradian: num 50
```

Because the default slope is 0, a horizontal line can be set with just
the intercept:

``` r
line(intercept = 3)
#> <line>
#>  @ a          : num 0
#>  @ b          : num 1
#>  @ c          : num -3
#>  @ slope      : num 0
#>  @ intercept  : num 3
#>  @ x_intercept: num -Inf
#>  @ angle      : <angle>
#>  .. @ turn   : num 0
#>  .. @ radian : num 0
#>  .. @ degree : num 0
#>  .. @ gradian: num 0
```

A vertical line can be set with the x- intercept:

``` r
line(x_intercept = 5)
#> <line>
#>  @ a          : num 1
#>  @ b          : num 0
#>  @ c          : num -5
#>  @ slope      : num -Inf
#>  @ intercept  : num Inf
#>  @ x_intercept: num 5
#>  @ angle      : <angle>
#>  .. @ turn   : num 0.75
#>  .. @ radian : num 4.71
#>  .. @ degree : num 270
#>  .. @ gradian: num 300
```

Any line can be constructed from the coefficients of this equation:

$$
ax+by+c=0
$$

``` r
line(a = 1, b = 2, c = 3)
#> <line>
#>  @ a          : num 1
#>  @ b          : num 2
#>  @ c          : num 3
#>  @ slope      : num -0.5
#>  @ intercept  : num -1.5
#>  @ x_intercept: num -3
#>  @ angle      : <angle>
#>  .. @ turn   : num 0.926
#>  .. @ radian : num 5.82
#>  .. @ degree : num 333
#>  .. @ gradian: num 370
```

### Segments

A segment is a portion of a line between two points.

``` r
p1 <- point(0,1)
p2 <- point(3,3)
s1 <- segment(p1, p2)
```

The line that passes between both points:

``` r
s1@line
#> <line>
#>  @ a          : num -2
#>  @ b          : num 3
#>  @ c          : num -3
#>  @ slope      : num 0.667
#>  @ intercept  : num 1
#>  @ x_intercept: num -1.5
#>  @ angle      : <angle>
#>  .. @ turn   : num 0.0936
#>  .. @ radian : num 0.588
#>  .. @ degree : num 33.7
#>  .. @ gradian: num 37.4
```

### Retangles

A rectangle has 4 corners (`northeast`, `northwest`, `southwest`, and
`southeast`). It has a center. It has width and height. If you give the
`rectangle` function enough information to calculate its four corners,
it will do so. All of the following will give the same rectangle:

``` r
ne <- point(4,2)
nw <- point(0,2)
sw <- point(0,0)
se <- point(4,0)
cent <- point(2,1)

# Give width, height, and any point
rectangle(width = 4, height = 2, center = cent)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r
rectangle(width = 4, height = 2, northeast = ne)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r
rectangle(width = 4, height = 2, northwest = nw)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r
rectangle(width = 4, height = 2, southwest = sw)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r
rectangle(width = 4, height = 2, southeast = se)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r

# Give the center and any of the 4 corners
rectangle(center = cent, northeast = ne)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r
rectangle(center = cent, northwest = nw)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r
rectangle(center = cent, southwest = sw)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r
rectangle(center = cent, southeast = se)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r

# Give opposite corners
rectangle(northeast = ne, southwest = sw)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r
rectangle(northwest = nw, southeast = se)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r

# Give width and two points either side
rectangle(width = 4, northwest = nw, southwest = sw)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r
rectangle(width = 4, northeast = ne, southeast = se)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r

# Give height and two points on top or bottom
rectangle(height = 2, northwest = nw, northeast = ne)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

``` r
rectangle(height = 2, southwest = sw, southeast = se)
#> <rectangle>
#>  @ center   : <point>
#>  .. @ x       : num 2
#>  .. @ y       : num 1
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 2.24
#>  .. @ xy      : num [1, 1:2] 2 1
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ width    : num 4
#>  @ height   : num 2
#>  @ northeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 2
#>  .. @ slope   : num 0.5
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.0738
#>  .. .. @ radian : num 0.464
#>  .. .. @ degree : num 26.6
#>  .. .. @ gradian: num 29.5
#>  .. @ distance: num 4.47
#>  .. @ xy      : num [1, 1:2] 4 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ northwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 2
#>  .. @ slope   : num Inf
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0.25
#>  .. .. @ radian : num 1.57
#>  .. .. @ degree : num 90
#>  .. .. @ gradian: num 100
#>  .. @ distance: num 2
#>  .. @ xy      : num [1, 1:2] 0 2
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southwest: <point>
#>  .. @ x       : num 0
#>  .. @ y       : num 0
#>  .. @ slope   : num NaN
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 0
#>  .. @ xy      : num [1, 1:2] 0 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ southeast: <point>
#>  .. @ x       : num 4
#>  .. @ y       : num 0
#>  .. @ slope   : num 0
#>  .. @ angle   : <angle>
#>  .. .. @ turn   : num 0
#>  .. .. @ radian : num 0
#>  .. .. @ degree : num 0
#>  .. .. @ gradian: num 0
#>  .. @ distance: num 4
#>  .. @ xy      : num [1, 1:2] 4 0
#>  .. .. - attr(*, "dimnames")=List of 2
#>  .. ..  ..$ : NULL
#>  .. ..  ..$ : chr [1:2] "x" "y"
#>  @ xy       : num [1:4, 1:2] 4 0 0 4 2 2 0 0
#>  .. - attr(*, "dimnames")=List of 2
#>  ..  ..$ : chr [1:4] "northeast" "northwest" "southwest" "southeast"
#>  ..  ..$ : chr [1:2] "x" "y"
```

## Methods

### Distances

The distance function can be used to find the distance between two
points.

``` r
distance(p1, p2)
#> [1] 3.605551
```

The distance of a point to the origin:

``` r
distance(p1)
#> [1] 1
```

The distance between the endpoints of segment:

``` r
distance(s1)
#> [1] 3.605551
```

The distance from a point to a line:

``` r
distance(p1, l1)
#> [1] 1.414214
```
