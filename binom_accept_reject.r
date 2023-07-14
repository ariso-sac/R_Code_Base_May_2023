# N <- 1e4
# y=rbinom(N,size = 10,prob = 0.5)
# plot(density(y))
n <- 20
p <- 0.5
x <- 0:n
all_c <- choose(n, x) * p^(x - 1) * (1 - p)^(n - 2 * x)
c <- max(all_c)
draw_binomial <- function(n, p) {
    count <- 0
    accept <- -1
    while (accept < 0) {
        count <- count + 1
        u <- runif(1)
        prop <- rgeom(1, prob = p)
        ratio <- dbinom(x = prop, size = n, prob = p) /
            (dgeom(x = prop, prob = p) * c)
        if (u < ratio) {
            return(c(prop, count))
        }
    }
}
N <- 1e4
s <- numeric(N)
counts <- numeric(N)
for (i in 1:N) {
    f <- draw_binomial(n, p)
    s[i] <- f[1]
    counts[i] <- f[2]
}
mean(s)
n * p
mean(counts)
c
plot(density(s))
