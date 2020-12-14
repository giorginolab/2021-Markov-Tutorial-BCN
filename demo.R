# The weather example
P <- matrix( c(0.6,  0.3,  0.1,
               0.2,  0.3,  0.5,
               0.4,  0.1,  0.5  ), 
             nrow = 3, byrow = T)


Q <- matrix( c(3/5,  0,  2/5,
               1/4, 3/4, 0,
               1/4, 2/4, 1/4  ), 
             nrow = 3, byrow = T)

# Remove imaginary part if small
unIm <- function(x) {
        if (all(Im(z <- zapsmall(x))==0)) as.numeric(z) else x
}

# Extract stationary state as eigenvector of eigenvalue=1
sstate <- function(M) {
        ev<-eigen(t(M))
        if(unIm(ev$values[1]) != 1) 
                stop("First EV not 1")
        ss <- ev$vectors[,1] / sum(ev$vectors[,1])
        unIm(ss)
}

# Sample a Markov chain 
sample.markov <- function(P, n, s0=1) {
        N <- nrow(P)
        s <- s0
        ret <- s
        for (i in 2:n) {
               p <- P[s, ] 
               nx <- sample(1:N, size=1, prob=p)
               ret <- c(ret, nx)
               s <- nx
        }
        return(ret)
}

# Compute time-dependent probabilities of a Markov chain
probs.markov <- function(P, n, s0 = c(1,rep(0,nrow(P)-1))) {
        N <- nrow(P)
        s <- s0
        ret <- s
        for (i in 2:n) {
                s <- s %*% P
                ret <- rbind(ret, s)
        }
        return(ret)
}


sample.markov(P, 30)

ss <- sstate(P)

N <- 1000
sN <- sample.markov(P, N)
plot(table(sN), xlab="State", ylab="Times visited")

points(1:3, N*ss, col=2)
