

t.o.C <- 26 # Temperature at which the observed density was measured (degC)
# Glass Thermal Expansion Correction for a Base Temperature (Tb) of 20C
c.glass.20 <- 1-0.000023*(t.o.C-20)-0.00000002*(t.o.C-20)^2 

# Gasohol: Special Application
# Gasohol E10 coeff of thermal expansion at 60F:
alpha.E10.60.F <- 714.34*10^(-6)
# F^(-1) (U.Missouri-Rolla). See MPMS.2004 11.1.2.5.13
# Consider estimating alpha.E25.60.F, as per 11.1.5.2

# Unit conversions MPMS 11.1.5.1
t.CF <- function(t.C) 1.8*t.C+32
t.FC <- function(t.F) (t.F-32)/1.8
p.kpapsig <- function(p.kpa) p.kpa/6.894757
p.barpsig <- function(p.bar) p.bar/0.06894757
rho.w.60 <- 999.016 #kg/m3
rho.relkgm3 <- function(gamma.60) gamma.60*rho.w.60
rho.apikgm3 <- function(g.api) (141.5/(g.api+131.5))*rho.w.60
gamma.kgm3rel <- function(rho.kgm3) rho.kgm3/rho.w.60
g.kgm3api <- function(rho.kgm3) 141.5/(rho.kgm3/rho.w.60)-131.5
alpha.CF <- function(alpha.C) alpha.C/1.8
alpha.FC <- function(alpha.F) alpha.F*1.8

## TEMPERATURE SCALES
# Ackowledge the difference in IPTS-68, used in the correlations from the
# MPMS Ch 11.1 - 1980 and the ITS-90, which is in use today:
# 1980: rho.star (IPTS-68). 2004: rho.60 (ITS-90)
# delta.60 compensates the difference
delta.60 <- 0.01374979547 # degF
# Temp Conversion from ITS-90 to IPTS-68 as per 11.1.5.3
a.i <- c(-0.148759,-0.267408,1.080760,1.269056,-4.089591,-1.871251,
                  7.438081,-3.536296)
t.68.C <- function(t.90.C) {
        tau <- t.90.C/630
        delta.t <- a.i[1]*tau+a.i[2]*tau^2+a.i[3]*tau^3+a.i[4]*tau^4+
                a.i[5]*tau^5+a.i[6]*tau^6+a.i[7]*tau^7+a.i[8]*tau^8
        t.90.C-delta.t
}


# Rounding 11.1.5.4
# round() IEC 60559 standard is expected to be used: "go to the even digit"
# round(5.34/0.05)*0.05
# round(-5.34/0.05)*0.05
# round(10.05/0.1)*0.1


###########################################################################
###########                        Ranges                       ###########
###########################################################################
range.t.F <- function(t.F) -58<=t.F & t.F<=302
range.p.psig <- function(p.psig) 0<=p.psig & p.psig<=1500
range.rho.60.kgm3 <- function(comodity.group,rho.60.kgm3,...) {
        if(comodity.group=="Crude Oil") {
                return(610.6<=rho.60.kgm3 & rho.60.kgm3<=1163.5)
        }
        if(comodity.group=="Fuel Oils") {
                return(838.3127<=rho.60.kgm3 & rho.60.kgm3<=1163.5)
        }
        if(comodity.group=="Jet Fuels") {
                return(787.5195<=rho.60.kgm3 & rho.60.kgm3<838.3127)
        }
        if(comodity.group=="Transition Zone") {
                return(770.3520<=rho.60.kgm3 & rho.60.kgm3<787.5195)
        }
        if(comodity.group=="Gasolines") {
                return(610.6<=rho.60.kgm3 & rho.60.kgm3<770.3520)
        }
        if(comodity.group=="Lubricating Oil") {
                800.9<=rho.60.kgm3 & rho.60.kgm3<=1163.5
        }
}
range.alpha.60 <- function(alpha.60) 230E-6<=alpha.60 & alpha.60<=930E-6
# if p<0, set p=0

###########################################################################
###########                  K Coefficients                     ###########
###########################################################################
# K coefficients--COMODITY GROUP
k.i <- function(comodity.group) {
        if(comodity.group=="Crude Oil") return(c(341.0957,0,0))
        if(comodity.group=="Fuel Oils") return(c(103.8720,.2701,0))
        if(comodity.group=="Jet Fuels") return(c(330.3010,0,0))
        if(comodity.group=="Transition Zone") return(c(1489.0670,0,-0.00186840))
        if(comodity.group=="Gasolines") return(c(192.4571,0.2438,0))
        if(comodity.group=="Lubricating Oils") return(c(0,0.34878,0))
}


###########################################################################
###########                        11.1.6.1                     ###########
###########################################################################
# Method to Correct a Measured Volume to Base Conditions and Density from Base
# Conditions to an Alternate Temperature and Pressure

mpms.11.1.6.1 <- function(...,comodity.group,rho.60.kgm3,
                          t.F,p.psig,alpha.60.F=NULL) {
        stopifnot(range.rho.60.kgm3(...))
        # if(range.rho.60(...))
        # if(is.null(alpha.60.F)) {
        #         t.C <- t.FC(t.F)
        #         t.star <- t.68.C(t.C)
        #         
        # } else {
        #         rho.star <- rho.60.kgm3*exp(.5*alpha.60.F*delta.60*
        #                                             (1+.4*alpha.60.F*delta.60))
        # }
}


