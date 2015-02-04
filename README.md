probability distributions 2

q1: dat1952 = gapminder[ gapminder$year == 1952, ]

    x = dat1952$lifeExp

    mean(x <= 40)


q2: dat1952 = gapminder[ gapminder$year == 1952, ]

     x = dat1952$lifeExp

     mean(x <= 60) - mean(x <= 40)


q3:dat1952 = gapminder[ gapminder$year == 1952, ]

   pop = dat1952$pop

   logpop = log10(pop)

   # the sample standard deviation

   sd(logpop)

   # the population standard deviation

   sqrt(mean((logpop - mean(logpop))^2))


q4: dat1952 = gapminder[ gapminder$year == 1952, ]

    pop = dat1952$pop

    x = log10(pop)

    z = (x - mean(x))/sd(x)

    tail(sort(z), 1) 


q5: dat1952 = gapminder[ gapminder$year == 1952, ]

    F = function(q) pnorm(q, mean=mean(x), sd=sd(x))

    n = length(x)

    n * (F(7) - F(6))

q6:  dat1952 = gapminder[ gapminder$year == 1952, ]
     x = log10(pop)
     n = length(x)
     ps = ((1:n) - 0.5)/n
     qnorm(ps)
