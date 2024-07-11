library(ggpathdiagramr)
library(testthat)

test_that("constructor", {
  aa <- point(c(2, 2, 3), c(4, 1, 3))
  bb <- c(point(2, 4), point(2, 1), point(3, 3))
  cc <- point(x = c(2, 3), y = 1)
  dd <- point(x = 1, y = c(2, 2))
  expect_identical(aa, bb)
  expect_identical(point(c(2,2)), point(2,2))
  expect_identical(point(distance = 1, angle = 0), point(x = 1, y = 0))
  l111 <- line(a = 1, b = 1, c = 1)
  expect_identical(l111, line(slope = -1, intercept = -1))
  v1 <- line(a = 1, b = 0, c = -1)
  expect_identical(v1, line(x_intercept = 1))
  h1 <- line(a = 0, b = 1, c = -1)
  expect_identical(h1, line(intercept = 1))
  expect_error(line(a = 1, b = 1, c = 1, slope = 3), "The slope is incompatible with parameters a, b, and c.")
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
})

test_that("perpendicular", {
  expect_identical(point(0,0) %-|% point(2,2), point(2,0))
  expect_identical(point(0,0) %|-% point(2,2), point(0,2))
})

test_that("distances", {
  p0 <- point(0,3)
  p1 <- point(1,1)
  p2 <- point(4,5)
  n1 <- node(p1, label = "A")
  n2 <- node(p2, "B")
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


test_that("angle", {
  p0 <- point(0,3)
  p1 <- point(1,1)
  p2 <- point(4,5)
  n1 <- node(p1, label = "A")
  n2 <- node(p2, "B")
  s1 <- segment(p1, p2)
  c1 <- circle(p1, radius = 1)
  c2 <- circle(p2, radius = 2)
  expect_equal(angle(p0), pi/2)
  expect_equal(angle(point(1,4), point(2,5)), pi/4)
  expect_equal(distance(s1), 5)
  expect_equal(distance(c1, c2), 2)
  expect_equal(distance(c1, c2, center = TRUE), 5)
  expect_equal(distance(c1, c1), 0)
})


test_that("segments", {
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
               c(p1, p2))
  expect_equal(intersection(c1, l1),
               c(p1, p2))
  expect_equal(intersection(line(0, 2), c1),
               point(1,2))

})






