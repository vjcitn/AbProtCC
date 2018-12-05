fig3w = function(g11=.1, g12=.5, g01=3, g02=.5, a=seq(0,3,.1),
   pi=.01, lty=2, rend=plot) {
  G = function(a, lam, nu) exp(-(a/lam)^nu)
  G1 = function(a) G(a, g11, g12)
  G0 = function(a) G(a, g01, g02)
  thet = function(a) pi*G1(a)/(pi*G1(a)+(1-pi)*G0(a))
  rend(a, sapply(a, thet), type="l", ylab="theta(a)", xlab="a", lty=lty)
}
   
fig3ln = function(g11 = -1.3, g12=2, g01=3.3, g02=.55, a=seq(0,3,.1),
  pi = 0.01, rend=lines) {
  Gw = function(a, lam, nu) exp(-(a/lam)^nu)
  Gln = function(a, m, s) 1-plnorm(a, m, s) # m s from logs of data
  G1 = function(a) Gln(a, g11, g12)
  G0 = function(a) Gw(a, g01, g02)
  thet = function(a) pi*G1(a)/(pi*G1(a)+(1-pi)*G0(a))
  rend(a, sapply(a, thet))
}

#' crude production of figure 3 from 2001 paper
#' @examples
#' fig3()
#' @export
fig3 = function() {
 fig3w() 
 fig3w(1, 4, 2, 4, lty=3, rend=lines)
 fig3ln()
}
