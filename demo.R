set.seed(191919)

source("functions.R")

# The weather example
weatherP <- matrix( c(0.6,  0.3,  0.1,
                      0.2,  0.3,  0.5,
                      0.4,  0.1,  0.5  ), 
                    nrow = 3, byrow = T)


# The "colored blocks" trajectory example
colortrajP <- matrix( c(3/5,  0,  2/5,
                        1/4, 3/4, 0,
                        1/4, 2/4, 1/4  ), 
                      nrow = 3, byrow = T)

P <- weatherP

# Extract 30 time steps
sample.markov(P, 30)

# Compute the stationary state via the eigenvectors
ss <- sstate(P)

# Check the stationary state by extracting 1000 samples
N <- 1000
sN <- sample.markov(P, N)
plot(table(sN), xlab="State", ylab="Times visited")

# Compare with the theory
points(1:3, N*ss, col=2)


# --------------

pT <- probs.markov(P, 8)
matplot(pT, type="o", 
        xlim = c(1,10),
        xlab="Time (starting from T=1)", 
        ylab="Probability of each state",
        main="Starting from state 1")
points(rep(10,3),ss,col=1:3)

pT <- probs.markov(P, 8, c(0,1,0))
matplot(pT, type="o", 
        xlim = c(1,10),
        xlab="Time (starting from T=1)", 
        ylab="Probability of each state",
        main="Starting from state 3")
points(rep(10,3),ss,col=1:3)

pT <- probs.markov(P, 8, c(0,0,1))
matplot(pT, type="o", 
        xlim = c(1,10),
        xlab="Time (starting from T=1)", 
        ylab="Probability of each state",
        main="Starting from state 3")
points(rep(10,3),ss,col=1:3)


