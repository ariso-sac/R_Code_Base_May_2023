N <- 1e4
# y=rbinom(n,size = 10,prob = 0.3)
# plot(density(y))
getbinom <- function(n, p) {
    x <- numeric(n)
    for (i in 1:n) {
        temp <- choose(n, i)
        temp <- temp * (p)^i * (1 - p)^(n - i)
        x[i] <- temp
    }
    return(x)
}
f <- getbinom(10, 0.5)
cdf <- function(n, l) {
    s <- numeric(n)
    for (i in 1:n) {
        s[i] <- sum(l[1:i])
    }
    return(s)
}
g <- cdf(10, f)
g
y <- numeric(N)
for (i in 1:N) {
    foo <- runif(1)
    for (k in 1:10) {
        if (foo < g[k]) {
            y[i] <- k
            break()
        }
    }
}
plot(density(y))
