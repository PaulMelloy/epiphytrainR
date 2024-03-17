#' Estimate disease Temperature range
#'
#' @param Tm numeric, the temperature at which the disease is being estimated
#' @param Tm_opt numeric, the optimum temperature for the disease
#' @param Tm_max numeric, the maximum temperature for the disease
#' @param Tm_min numeric, the minimum temperature for the disease
#' @param k numeric, dimensionless factor controlling the shape of the curve
#'
#' @return numeric, the probability of disease growth
#' @export
#'
#' @examples
#' d <- Tm_range(0:30,Tm_opt = 20, Tm_max = 28, Tm_min = 0)
#' plot(x = 0:30, y = d, type = "l")
Tm_range <- function(Tm,Tm_opt = 26, Tm_max =35, Tm_min=10, stdev = 1){
   #wght <- dnorm(seq(Tm_min - Tm_opt,Tm_max - Tm_opt,by=0.1),sd = rng/3)
   # difference between optimum Tm and minimum Tm
   wt_l <- Tm_opt - Tm_min
   # difference between optimum Tm and maximum Tm
   wt_u <- Tm_max - Tm_opt

   rng <- Tm_max - Tm_min

   low_dist <- 1 - dnorm(seq(0,wt_l,by=0.1),sd = wt_l/3)
   low_dist <- max(low_dist) - low_dist
   low_scale_factor <- 1/max(low_dist)
   high_dist <- dnorm(seq(0,wt_u,by=0.1),sd = wt_u/3)
   high_scale_factor <- 1/max(high_dist)

   scale_it <-
      dnorm(Tm_opt,
            mean = Tm_opt,
            sd = ((rng) /5)*stdev)*15.039

   out <-
      sapply(Tm,function(xt){
         if(xt< Tm_min |
            xt > Tm_max) {
            return(0)
         }
         wt_o <- abs(xt - Tm_opt)

         Tm_dist <-
            dnorm(xt,
                  mean = Tm_opt,
                  sd = ((rng) /5)*stdev)*15.039

         if(xt < Tm_opt) {
            wght <- dnorm(wt_o,sd = wt_l/3) * low_scale_factor}
         if(xt > Tm_opt){
               wght <- dnorm(wt_o,sd = wt_u/3) * high_scale_factor
         }
         if(xt == Tm_opt){
            wght <- 1
         }

         fctr <- Tm_dist * wght
         return(fctr )
      })

   # m_out <- max(range_dist(seq(Tm_min,
   #                           Tm_max,
   #                           by = 0.1),
   #                       Tm_opt, Tm_max, Tm_min, stdev, low_scale_factor, high_scale_factor))

   return(out/scale_it)



}


# Testing the function

# dat <-
#    data.frame(x = 0,
#            o = 14,
#            ma = 28,
#            mi = 0,
#            out = Tm_range(0,Tm_opt = 14, Tm_max = 28, Tm_min = 0)
#            )
# dat2 <- vector("list", 51*16*16*16)
#  i <- 1
# for(x in 0:50){
#    for(o in 15:29){
#       for(ma in 29:45){
#          for(mi in 0:15){
#             dat2[[i]] <-
#                data.frame(x = x,
#                              o = o,
#                              ma = ma,
#                              mi = mi,
#                              out = Tm_range(x,Tm_opt = o, Tm_max = ma, Tm_min = mi)
#                              )
#             i <- i + 1
#
#       }
#    }
# }
# }
# dat3 <- do.call(rbind,dat2)
# summary(dat3$out)
