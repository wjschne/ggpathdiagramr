library(ggpathdiagramr)
library(testthat)

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
  l1 <- line(slope = 0, intercept = 1)
  p1 <- point(0, 1)
  p2 <- point(2, 1)
  expect_equal(intersection(l1, c1),
               c(p1, p2))
  expect_equal(intersection(c1, l1),
               c(p1, p2))
  expect_equal(intersection(line(0, 2), c1),
               point(1,2))

})






