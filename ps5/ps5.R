library(data.table)

# 2.
options(digits = 22)
# exactly
2^53-1

# exactly
2^53

# not exactly
2^53+1


# 3.(a)
int <- as.integer(round(rnorm(100000000)))
num <- rnorm(100000000)
system.time(intCopy <- copy(int))
system.time(numCopy <- copy(num))


# 3.(b)
l <- length(int)/2
system.time(sample(int, size = l, replace = FALSE))
system.time(sample(num, size = l, replace = FALSE))


# 5.
0.2 + 0.3 == 0.5
0.02 + 0.48 == 0.5
0.01 + 0.49 == 0.5
0.2 + 0.1 == 0.3