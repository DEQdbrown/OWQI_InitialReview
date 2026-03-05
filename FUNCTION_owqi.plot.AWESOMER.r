#12 graphs (3 pages) of OWQI (graph 1) + OWQI, parameter, and subindex graphs for 11 parameters
#See 'w.all.avg.csv' for formatting of input file; created by R.Leferink 2/1/11 Edited by P. Bryant 1/27/14 to ouput to year specific folder
#No output appears in R; writes to .pdf files in destination folder under 30 yr graphs folder in the owqi folder on the share drive

owqi.plot<-function(data,x,y, endWY){
        site1<-as.vector(data$combo_name[1])                 
        station1<-as.vector(data$Station[1])             
        pdf(file=paste('//deqlab1/wqm/owqi/30yr graphs/',endWY,'/','ZZZ',site1, " #", station1, ", ", endWY-30, "-", endWY, ".pdf", sep=""), #Formerly lead01...#
            width=11, height=8.5)
        par(mfrow=c(2,2), mar=c(4,4,4,4), oma=c(.4, 0,1.7, 0))

#page 1
#OWQI graph        
  plot(with(data, y~x), col='white', ylim = c(10,100), xlim=c(endWY-30,endWY),
      main="OWQI", font.main=2, xlab="Water Year", ylab="")
      minor.tick(nx=5, ny=5, tick.ratio=1); abline(h=80,col="lightgray", lwd=2, lty=2)
      abline(h=85,col="lightgray", lwd=2, lty=2); abline(h=90,col="lightgray", lwd=2, lty=2)
      abline(v=endWY-10, col="gray", lwd=2)
      lines(with(data, y~x), col=1, lwd=3)
      mtext("OWQI", cex=.9, side=2, line=2)  
      
      mtext(text=site1, line=-0.4, font=2, cex=1.2, outer=TRUE)
      mtext("Page 1 of 3", side=1, line=-1, font=2, cex=.75, outer=TRUE)
      text(endWY-29, 92, "Excellent", col="gray"); text(endWY-29, 87, "Good", col="gray"); text(endWY-29, 82, "Fair", col="gray"); text(endWY-29, 77, "Poor", col="gray")
  
        
#temp
  plot(y~x, type="l", ylim = c(10,100), xlim=c(endWY-30, endWY), col="#CED1D8", lwd=5,
    main="Temperature Subindex", font.main=2, ylab="", xlab="Water Year")
    lines(data$temp_si~x, lwd=2, col="#4568B5", lty=6);  axis(4, col.axis="#767592")
        abline(v=endWY-10, col="gray", lwd=2)
        mtext("Temp Subindex / OWQI", cex=.9, side=2, line=2)  
    legend("bottomright", c("OWQI", "Temp Subindex"), col=c("#CED1D8","#4568B5"),
       lty=c(1, 6), lwd=c(5, 2), bty="n", cex=.8)
     
     
#do
  plot(y~x, type="l", ylim = c(10,100), xlim=c(endWY-30, endWY), col="#CED1D8", lwd=5,
    main="Dissolved Oxygen Subindex", font=2, ylab="", xlab="Water Year")
    lines(data$do_si~x, lwd=2, col="#4568B5", lty=6);  axis(4, col.axis="#767592") 
        abline(v=endWY-10, col="gray", lwd=2)
         mtext("DO Subindex / OWQI", cex=.9, side=2, line=2)
    legend("bottomright", c("OWQI", "DO Subindex"), col=c("#CED1D8","#4568B5"),
       lty=c(1, 6), lwd=c(5, 2), bty="n", cex=.8)


#bod5
  plot(y~x, type="l", ylim=c(10, 100), xlim=c(endWY-30, endWY), col="#CED1D8", lwd=5, 
    main="Biochemical Oxygen Demand Subindex", font.main=2, ylab="", xlab="Water Year")
    lines(data$bod_si~x, lwd=2, col="#4568B5", lty=6);  axis(4, col.axis="#767592")
        abline(v=endWY-10, col="gray", lwd=2)
         mtext("BOD Subindex / OWQI", cex=.9, side=2, line=2)
        legend("topright", c("OWQI", "BOD Subindex"), col=c("#CED1D8","#4568B5"),
      lty=c(1, 6), lwd=c(5, 2), bty="n", cex=.8)


#page 2
#ph
  plot(y~x, type="l", ylim=c(10, 100), xlim=c(endWY-30, endWY), col="#CED1D8", lwd=5, 
    main="pH", font.main=2, ylab="", xlab="Water Year")
    lines(data$ph_si~x, lwd=2, col="#4568B5", lty=6);  axis(4, col.axis="#767592")
        abline(v=endWY-10, col="gray", lwd=2)  
         mtext("pH Subindex / OWQI", cex=.9, side=2, line=2)
    legend("bottomright", c("OWQI", "pH Subindex"), col=c("#CED1D8","#4568B5"),
      lty=c(1, 6), lwd=c(5, 2), bty="n", cex=.8)
      
        mtext(text=site1, line=-0.4, font=2, cex=1.2, outer=TRUE)
    mtext("Page 2 of 3", side=1, line=-1, font=2, cex=.75, outer=TRUE) 
         
    
#ts   
  plot(y~x, type="l", ylim=c(10, 100), xlim=c(endWY-30, endWY), col="#CED1D8", lwd=5, 
    main="Total Solids Subindex", font.main=2, ylab="", xlab="Water Year")
    lines(data$ts_si~x, lwd=2, col="#4568B5", lty=6);  axis(4, col.axis="#767592") 
        abline(v=endWY-10, col="gray", lwd=2)
         mtext("Total Solids Subindex / OWQI", cex=.9, side=2, line=2)
    
    legend("bottomleft", c("OWQI", "T Solids Subindex"), col=c("#CED1D8","#4568B5"),
       lty=c(1, 6), lwd=c(5, 2), bty="n", cex=.8)  
    
#fecal
  plot(y~x, type="l", ylim=c(10, 100), xlim=c(endWY-30, endWY), col="#CED1D8", lwd=5, 
    main="Bacteria Subindex", font.main=2, ylab="", xlab="Water Year")
    lines(data$bact_si~x, lwd=2, col="#4568B5", lty=6);  axis(4, col.axis="#767592") 
       abline(v=endWY-10, col="gray", lwd=2)
        mtext("Bacteria Subindex / OWQI", cex=.9, side=2, line=2)
    legend("bottomright", c("OWQI", "Bact Subindex"), col=c("#CED1D8","#4568B5"),
       lty=c(1, 6), lwd=c(5, 2), bty="n", cex=.8)


 #ntotal.n/ n_si  
  plot(y~x, type="l", ylim=c(10, 100), xlim=c(endWY-30, endWY), col="#CED1D8", lwd=5, 
    main="Nitrogen Subindex", font.main=2, ylab="", xlab="Water Year")
    lines(data$n_si~x, lwd=2, col="#4568B5", lty=6);  axis(4, col.axis="#767592") 
        abline(v=endWY-10, col="gray", lwd=2)
    minor.tick(nx=5, tick.ratio=1) 
     mtext("N Subindex / OWQI", cex=.9, side=2, line=2)
    
    legend("topright", c("OWQI", "N Subindex"), col=c("#CED1D8","#4568B5"),
       lty=c(1, 6), lwd=c(5, 2), bty="n", cex=.8)
      
      

       
#page 3

     
#p
  plot(y~x, type="l", ylim=c(10, 100), xlim=c(endWY-30, endWY), col="#CED1D8", lwd=5, 
    main="Phosphorus Subindex", font.main=2, ylab="", xlab="Water Year")
    lines(data$p_si~x, lwd=2, col="#4568B5", lty=6);  axis(4, col.axis="#767592") 
    minor.tick(nx=5, tick.ratio=1) 
         abline(v=endWY-10, col="gray", lwd=2)
         mtext("P Subindex / OWQI", cex=.9, side=2, line=2)
    legend("topright", c("OWQI", "P Subindex"), col=c("#CED1D8","#4568B5"),
       lty=c(1, 6), lwd=c(5, 2), bty="n", cex=.8)  

       
       
    mtext(text=site1, line=-0.4, font=2, cex=1.2, outer=TRUE)
    mtext("Page 3 of 3", side=1, line=-1, font=2, cex=.75, outer=TRUE) 
     
     

        

    dev.off()
    windows.options(reset=TRUE)
    
      }
      
      #ecoli    
# plot(y~x, type="l", ylim=c(10, 100), xlim=c(endWY-30, endWY), col="white", lwd=5, 
#    main="", font.main=2, ylab="", xlab="")
#    lines(data$bact_si~x, lwd=2, col="white", lty=6);  axis(4, col.axis="white") 
      



        
