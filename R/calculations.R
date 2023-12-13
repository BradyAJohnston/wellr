
v1 <- function(c1, c2, v2) {
  c2 * v2 / c1
}

c1 <- function(v1, c2, v2) {
  c2 * v2 / v1
}

c2 <- function(c1, v1, v2) {
  c1 * v1 / v2
}

v2 <- function(c1, v1, c2) {
  c1 * v1 / c2
}
