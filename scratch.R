
library(ggpathdiagramr)

point(c(2,3))

angle(point(2,2))
angle(point(0,0), point(2,2))
angle(segment(point(0,0), point(2,2)))
distance(point(0,0), point(2,2))




intersection(point(2,2), line(slope = 1, intercept = 0))


x <- point(4 - 2, 2 - 1)
y <- line(slope = 1, intercept = 0)
length(intersection(x,y))
s <- segment(p1 = point(0,0), p2 = point(2,2))
s1 <- segment(point(0, 1), point(1, 0))
s2 <- segment(point(0, 0), point(1, 1))
intersection(s, point(1,3))
intersection(x = point(.5,.5), y = s1@line)

intersection(s1, s2)
intersection(s1, s2@line)
intersection(s1@line, s2)
intersection(s1@line, s2@line)
intersection(intersection(s1@line, s2@line), s1)
intersection(s1, s2 + point(1,1))



s1 <- segment(p1 = point(1,1))
angle(s1)
s1@slope <- 0
s1
s1@distance <- 10
s1
s1@angle <- pi / 2
s1


s2 <- segment(point(2,1))


x <- c(s1,s2)



p1 <- point(0,0)
c1 <- circle(p1, 1)
c2 <- circle(point(3,4))
distance(c1,c2)
distance(c1,c2, center = TRUE)
distance(p1, c2)
d <- c2@center - c1@center
angle(d)
p1 <- anchor(c1, angle(d), radians = T)
p2 <- anchor(c2, angle(d) + pi, radians = T)
distance(p1, p2)
distance(c1, c2)



mycircle = circle(point(x = 1, y = 2), radius = 2, n = 360)
mycircle


anchor(mycircle, position = "southeast")
anchor(mycircle, position = 250)

node(point(1,2),"sdf")
node(1,2,"sdf")
node(point(1,2))
aa <- point(x = c(2, 2, 3), y = c(4, 1, 3))
bb <- c(point(2, 4), point(2, 1), point(3, 3))
cc <- point(x = c(2, 3), y = 1)
dd <- point(x = 1, y = c(2, 2))
all.equal(aa,bb)

