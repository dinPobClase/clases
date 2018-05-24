
#----- CAP. Interacciones-----
# Predador-Presa (Lotka y Volterra)

plotLV <- function(N1,
                   N2,
                   alpha,
                   beta,
                   r1,
                   r2,
                   K1,
                   K2,
                   t,
                   dt = 1 /12) {
  pop1 <- numeric()
  pop2 <- numeric()
  timeLine <- 0
  pop1[1] <- floor(N1)
  pop2[1] <- floor(N2)
  
  
  i <- 0
  timeLine <- i
  while (i <= t) {
    N1. <- pop1[length(pop1)]
    N2. <- pop2[length(pop2)]
    
    N1. <- N1. + r1 * N1. * ((K1 + beta * N2. - N1.) / K1) * dt
    N2. <- N2. + r2 * N2. * ((K2 + alpha*N1. - N2.) / K2) * dt
    
    pop1 <- append(pop1, ifelse(N1. >= 0, N1., 0))
    pop2 <- append(pop2, ifelse(N2. >= 0, N2., 0))
    
    i <- i + dt
    timeLine <- append(timeLine,i)
  }
  par(oma = c(2,2,1,2))
  plot(
    c(0, t),
    range(c(pop1, pop2)),
    axes = F,
    type = "n",
    xlab = "tiempo",
    ylab = "Tamaño de población"
  )
  lines(timeLine,pop1, lty = 1)
  axis(side = 1)
  axis(side = 2)
  par(new = TRUE)
  plot(
    timeLine,
    pop2,
    lty = 2,
    axes = FALSE,
    xlab = "",
    ylab = "",
    type = "l"
  )
  axis(side = 4)
  mtext("Coyote", side = 4, line = 2)
  legend("right",lty = c(1,2), legend = c("presa","predador"))
  abline(h = 0, lwd=3)
  box(which = "plot")
}
