library(ggpathdiagramr)
library(testthat)

test_that("constructor", {
  # Point
  a <- point(0,0)
  a@x <- 3
  expect_identical(a, point(3,0))
  aa <- point(c(2, 2, 3), c(4, 1, 3))
  bb <- point_list(c(point(2, 4), point(2, 1), point(3, 3)))
  cc <- point(x = c(2, 3), y = 1)
  dd <- point(x = 1, y = c(2, 2))
  expect_identical(aa, bb)
  expect_identical(point(c(2,2)), point(2,2))
  expect_identical(point(distance = 1, angle = 0), point(x = 1, y = 0))
  expect_identical(bb@x, c(2, 2, 3))
  expect_identical(bb@y, c(4, 1, 3))
  expect_identical(bb@xy, cbind(x = c(2, 2, 3),
                                y = c(4, 1, 3)))
  expect_identical(bb@slope, c(2, 0.5, 1))
  expect_identical(bb@angle, angle(turn = atan(c(2, 0.5, 1)) / (2 * pi)))
  expect_identical(bb@distance, c(sqrt(20), sqrt(5), sqrt(18)))
  expect_identical(aa, point(cbind(aa@x, aa@y)))

  # Line
  l111 <- line(a = 1, b = 1, c = 1)
  expect_identical(l111, line(slope = -1, intercept = -1))
  v1 <- line(a = 1, b = 0, c = -1)
  expect_identical(v1, line(x_intercept = 1))
  h1 <- line(a = 0, b = 1, c = -1)
  expect_identical(h1, line(intercept = 1))
  expect_error(line(a = 1, b = 1, c = 1, slope = 3), "The slope is incompatible with parameters a, b, and c.")
  expect_error(line(slope = Inf, intercept = Inf), "There is not enough information to make a line. Specify the x-intercept or the a,b,c parameters.")
  expect_error(line(intercept = Inf), "There is not enough information to make a line. Specify the x-intercept or the a,b,c parameters.")
  expect_error(line(slope = Inf), "There is insufficient information to create a line.")

  # Angle
  t <- angle(1)
  expect_equal(angle(1)@degree, 0)
  expect_equal(angle(2)@degree, 0)
  expect_equal(angle(.5)@degree, 180)
  expect_equal(angle(.5)@radian, pi)
  expect_equal(angle(radian = pi)@radian, pi)
  expect_equal(angle(degree = 180)@radian, pi)
  # Rectangle
  p_center <- point(0,0)
  p_northeast <- point(2,1)
  p_northwest <- point(-2,1)
  p_southwest <- point(-2,-1)
  p_southeast <- point(2,-1)
  width <- 4
  height <- 2
  r_center <- rectangle(center = p_center, width = width, height = height)
  expect_identical(r_center, rectangle(center = p_center, northeast = p_northeast))
  expect_identical(r_center, rectangle(center = p_center, northwest = p_northwest))
  expect_identical(r_center, rectangle(center = p_center, southeast = p_southeast))
  expect_identical(r_center, rectangle(center = p_center, southwest = p_southwest))
  expect_identical(r_center, rectangle(width = width, height = height, southwest = p_southwest))
  expect_identical(r_center, rectangle(width = width, height = height, northwest = p_northwest))
  expect_identical(r_center, rectangle(width = width, height = height, southeast = p_southeast))
  expect_identical(r_center, rectangle(width = width, height = height, northeast = p_northeast))
  expect_identical(r_center, rectangle(width = width, southeast = p_southeast, northeast = p_northeast))
  expect_identical(r_center, rectangle(width = width, southwest = p_southwest, northwest = p_northwest))
  expect_identical(r_center, rectangle(southwest = p_southwest, northeast = p_northeast))
  expect_identical(r_center, rectangle(northwest = p_northwest, southeast = p_southeast))
  expect_identical(r_center, rectangle(height = height, southwest = p_southwest, southeast = p_southeast))
  expect_identical(r_center, rectangle(height = height, northwest = p_northwest, northeast = p_northeast))
  expect_error(rectangle(width = width, southwest = p_southwest), "There is not enough information to make a rectangle.")

  # segment
  segment(point(0,2), point(3,4))
})


test_that("adding",{
  p1 <- point(1,1)
  p2 <- point(3,4)
  p3 <- point(4,5)
  expect_identical(p1 + p2, p3)
  expect_identical(p3 - p2, p1)
  expect_identical(segment(p1, p2) + p3, segment(p1 + p3, p2 + p3))
  expect_identical(p3 + segment(p1, p2), segment(p1 + p3, p2 + p3))
  expect_identical(p3 - segment(p1, p2), segment(p3 - p1, p3 - p2))
  expect_identical(circle(p1, 2) + p2, circle(p1 + p2, 2))
  expect_identical(circle(p1, 2) - p2, circle(p1 - p2, 2))
  expect_identical(p2 + circle(p1, 2), circle(p1 + p2, 2))
  expect_identical(p2 - circle(p1, 2), circle(p2 - p1, 2))
  expect_identical(p2 + 2, point(5,6))
  expect_identical(2 + p2, point(5,6))
  expect_identical(p2 - 2, point(1,2))
  expect_identical(2 - p2, point(-1,-2))
})




test_that("multiplication",{
  p1 <- point(3,4)
  expect_identical(p1 * 2, point(6,8))
  expect_identical(2 * p1, point(6,8))
  expect_identical(p1 / 2, point(1.5,2))
  expect_identical(2 / p1, point(2/3,0.5))
})

test_that("perpendicular", {
  expect_identical(point(0,0) %-|% point(2,2), point(2,0))
  expect_identical(point(0,0) %|-% point(2,2), point(0,2))
})

test_that("distances", {
  p0 <- point(0,3)
  p1 <- point(1,1)
  p2 <- point(4,5)
  n1 <- label(p1, label = "A")
  n2 <- label(p2, "B")
  s1 <- segment(p1, p2)
  c1 <- circle(p1, radius = 1)
  c2 <- circle(p2, radius = 2)
  expect_equal(distance(p0), 3)
  expect_equal(distance(p1, p2), 5)
  expect_equal(distance(s1), 5)
  expect_equal(distance(c1, c2), 2)
  expect_equal(distance(c1, c2, center = TRUE), 5)
  expect_equal(distance(c1, c1), 0)
})


# test_that("angle", {
#   p0 <- point(0,3)
#   p1 <- point(1,1)
#   p2 <- point(4,5)
#   n1 <- label(p1, label = "A")
#   n2 <- label(p2, "B")
#   s1 <- segment(p1, p2)
#   c1 <- circle(p1, radius = 1)
#   c2 <- circle(p2, radius = 2)
#   expect_equal(angle(p0), pi/2)
#   expect_equal(angle(point(1,4), point(2,5)), pi/4)
#   expect_equal(distance(s1), 5)
#   expect_equal(distance(c1, c2), 2)
#   expect_equal(distance(c1, c2, center = TRUE), 5)
#   expect_equal(distance(c1, c1), 0)
# })


test_that("segments", {
  p1 <- point(0,3)
  p2 <- point(1,1)
  s1 <- segment(p1, p2)
  expect_equal(s1@line@intercept, 3)
})

test_that("segment_list", {
  p1 <- point(0,3)
  p2 <- point(1,1)
  s1 <- segment(p1, p2)
  expect_equal(s1@line@intercept, 3)
})



test_that("intersection", {
  s1 <- segment(point(0, 1), point(1, 0))
  s2 <- segment(point(0, 0), point(1, 1))
  s3 <- segment(point(0.1, .1), point(.9, .9))
  s4 <- segment(point(1, 1), point(2, 2))
  p1 <- point(.5, .5)
  expect_equal(intersection(s1,s2), p1)
  expect_equal(intersection(s1@line,s2), p1)
  expect_equal(intersection(s1,s2@line), p1)
  expect_equal(intersection(s1,s3), p1)
  expect_equal(intersection(s1,s4), list())
  expect_equal(intersection(s1@line, s4@line), p1)
  c1 <- circle(center = point(1, 1), radius = 1)
  l1 <- line(intercept = 1)
  p1 <- point(0, 1)
  p2 <- point(2, 1)
  expect_equal(intersection(l1, c1),
               c(p2, p1))
  expect_equal(intersection(c1, l1),
               c(p2, p1))
  expect_equal(intersection(line(intercept = 2), c1),
               point(1,2))
  l1 <- line(x_intercept = 1)
  p1 <- point(1, 0)
  p2 <- point(1, 2)
  expect_equal(intersection(l1, c1),
               c(p1, p2))
  expect_equal(intersection(c1, l1),
               c(p1, p2))
  expect_equal(intersection(line(x_intercept = 2), c1),
               point(2,1))
  l1 <- line(slope = 1, intercept = 2 * sin(pi / 4))
  c1 <- circle(point(0,0), radius = 1)
  expect_equal(intersection(l1, c1),
               point(angle = pi * 3 / 4, distance = 1))
  l1 <- line(slope = .5, intercept = 0)
  c1 <- circle(point(0,0), radius = 1)
  expect_equal(intersection(l1, c1),
               c(point(x = cos(atan(.5)), y = sin(atan(.5))),
                 point(x = -cos(atan(.5)), y = -sin(atan(.5)))
                 ))
  e1 <- ellipse(a = 1, b = 2)
  # plot(e1)
  # lines(s5@xy)
  s5 <- segment(point(-2,-2), point(2,2))
  intersection(s2, e1)
  intersection(s5, e1)
  intersection(segment(point(0,0), point(2,0)), segment(point(0,1), point(2,1)))

  intersection(segment(point(1,0), point(1,2)), segment(point(0,1), point(2,1)))
  intersection(segment(point(1,0), point(1,2)), segment(point(1,1), point(1,2)))
  intersection(line(1,intercept = 0), rectangle(point(0,0), width = 2, height = 2))
  intersection(segment(point(0,-3), point(0,10)), rectangle(center = point(0,0), width = 2, height = 2))
x <- segment(point(1,0), point(1,2))
y <- segment(point(1,1), point(1,2))
})



test_that("dotproduct", {
  p1 <- point(0, 1)
  p2 <- point(1, 0)
  expect_equal(p1 %*% p2, c(1,0) %*% c(0,1))
})


test_that("trig", {
  expect_equal(cos(angle(1)), cospi(2))
  expect_equal(sin(angle(.25)), sinpi(.5))
  expect_equal(tan(angle(.5)), tanpi(1))
})

test_that("rotate", {
  # rotate a line with an angle
  expect_identical(
    rotate(line(x_intercept = 2), angle(.5)),
    line(x_intercept = -2)
  )
  # rotate a line with a numeric radian
  expect_identical(
    rotate(line(x_intercept = 2), angle(.5)),
    rotate(line(x_intercept = 2), pi)
  )

  # rotate a point
  expect_identical(
    rotate(point(1,0), angle(.5)),
    point(-1,0)
  )

  # rotate a segment
  expect_identical(
    rotate(segment(point(0,1), point(1,0)), theta = angle(.5)),
    segment(point(0,-1), point(-1,0))
  )

  # rotate a circle
  expect_identical(
    rotate(x = circle(point(1, 2)),
           theta = angle(.25)),
    circle(point(-2, 1)))

  expect_identical(
    rotate(x = circle(point(1, 2), n = 50),
           theta = angle(.25)),
    circle(point(-2, 1), n = 50))

  # rotate an ellipse
  expect_identical(
    rotate(x = ellipse(center = point(1, 2), a = 2, b = 1),
           theta = angle(.25)),
    ellipse(point(-2, 1), a = 2, b = 1, theta = angle(.25)))
})

test_that("resect", {
  s1 <- segment(point(0,0), point(1,0))
  expect_identical(resect(s1, distance(s1) * .05),segment(point(.05,0), point(.95,0)))
  expect_identical(resect(point(0,1), .05), point(0, .95))
})

test_that("inside", {
  o <- point(0,0)
  r1 <- rectangle(center = o, width = 2, height = 4)
  expect_equal(inside(o,r1), 1)
  expect_equal(inside(point(0,2),r1), 0)
  expect_equal(inside(point(0,1.5),r1), 1)
  expect_equal(inside(point(1.5,1.5),r1), -1)
  expect_equal(inside(point(.5,2.5),r1), -1)

  c1 <- circle(point(1,1), radius = sqrt(2))
  expect_equal(inside(point(1,1), c1), 1)
  expect_equal(inside(o, c1), 0)
  expect_equal(inside(point(1,0), c1), 1)
  expect_equal(inside(point(1,2), c1), 1)
  expect_equal(inside(point(2,1), c1), 1)
  expect_equal(inside(point(2,2), c1), 0)
  expect_equal(inside(point(2,3), c1), -1)

  e1 <- ellipse(o, a = 1, b = 2)
  expect_equal(inside(o, e1), 1)
  re1 <- ellipse(o, a = 1, b = 2, theta = angle(degree = 45))
  # plot(NULL, xlim = c(-2,2), ylim = c(-2,2))
  # plot(re1, bor = "royalblue")
  # plot(e1, bor = "firebrick")
  # plot(o)
  p1 <- point(0,1.9)
  rp1 <- rotate(p1, angle(degree = 45))
  # plot(p1, col = "firebrick")
  # plot(rp1, col = "royalblue")
  expect_equal(inside(p1, e1), 1)
  expect_equal(inside(p1, re1), -1)
  expect_equal(inside(rp1, e1), -1)
  expect_equal(inside(rp1, re1), 1)
  # expect_equal(inside(point_list(c(o,p1,rp1, anchor(e1, angle(0)))), e1), c(1, 1, -1, 0))

})

