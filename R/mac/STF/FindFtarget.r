


# function to use in the optimisation to find the appropriate F to apply in the advice year,
# taking into account the AdviceRule


FindFtarget<- function(Fm,stk,srr,ady,cty,op)
{
#  stk=Mac.proj ; srr=Mac.srr ; ady=AdY ; cty=CtY ; op=c(0.21 , 2.57e6)  ; Fm=0.5

harvest(stk)[,ac(ady)]   <- Fm * harvest(stk)[,ac(ady-1)]

Fbar_AdY <- quantMeans(harvest(stk)[ac(4:8),ac(ady)])

 pro.ct  <- fwdControl(data.frame(year=c(ady,cty),
                                          quant=c("f","f"),
                                          value=c(Fbar_AdY ,Fbar_AdY )))


projj<-fwd(stk,control = pro.ct,sr= srr)

SSB_ady  <-  ssb(projj)[,ac(ady)]

Ftrg  <- op[1]
Btrg  <- op[2]


if(SSB_ady < Btrg) resA <- Ftrg* SSB_ady / Btrg
                    if(SSB_ady > Btrg) resA <- Ftrg

     ret       <- c((Fbar_AdY-resA)^2)

                 return(ret)
}