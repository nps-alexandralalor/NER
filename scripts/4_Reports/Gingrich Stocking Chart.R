"plot.gingrich.metric" = function(dm=c(15,20,25,30,35,40,45,50),type = "p",tpha=0, ba=0, ba.rng=c(0,36),tpha.rng=c(0,1600),a=c(-0.02053,0.02707,0.00199),b=c(0.070860,0.032680,0.003766),adj=1.0,amd.coef=c(-0.6579,0.973),label="",main="") {
  #
  #  Routine to plot a Gingrich stocking diagram
  #    requires the "qmd" and "stocking" routines
  #    metric version June 8, 2016
  #
  #  by David Larsen, Copyright June 11, 1993
  par(pty="s",lab=c(16,16,18))
  matplot(tpha,ba,type="n",xlim=tpha.rng,ylim=ba.rng,
          xlab="Density (trees/hectare)", ylab="Basal Area (sq. m^2/hectare)",main=main)
  # Uncomment for grid lines
  #axis(1, tck=1, ,lty=3, col="black")
  #axis(2, tck=1, ,lty=3, col="black")
  par(lty=1,col="black")
  box()
  # Draw QMD lines
  for( i in 1:(length(dm)) ){
    lines( qmdm( dm[i],adj=adj,a=a,amd.coef=amd.coef ))
  }
  # Draw stocking lines
  for( i in seq(10,110,10) ){
    lines( stocking.metric(dm=dm,b=b,a=a, percent=i,adj=adj,amd.coef=amd.coef ))
    labs<-stocking.metric(dm=dm[1],b=b,a=a, percent=i, adj=adj,amd.coef=amd.coef)
    text( labs$x+70,labs$y, paste(as.character(i),"%"),cex=0.75)
  }
  text( tpha.rng[2]/2, ba.rng[2]*.95,"Quadratic Mean Diameter",adj=0)
  text( tpha.rng[2]/1.5, ba.rng[2]*.92,"(cm)",cex=0.7,adj=0)
  labels<-as.character(dm)
  labs<-stocking.metric(dm=dm,b=b,a=a,percent=113,adj=adj,amd.coef=amd.coef)
  text( labs$x,labs$y,labels,cex=0.75)
  labs<-stocking.metric(dm=dm,b=b,a=a,adj=adj,amd.coef=amd.coef)
  text( labs$x[length(dm)]-85, labs$y[length(dm)],"A-line")
  lines(stocking.metric(dm=dm,b=b,a=a,percent=100,amd.coef=amd.coef),lwd=2)
  labs<-stocking.metric(dm=dm,b=b,a=a, b.line=T,amd.coef=amd.coef)
  text( labs$x[length(dm)]-70, labs$y[length(dm)],"B-line")
  lines(stocking.metric(dm=dm,b=b,a=a,b.line=T,amd.coef=amd.coef),lwd=2)
  # :xDraw the user data
  if( (is.vector(tpha) || is.matrix(tpha)) ){
    if(type == "p") {
      matpoints(tpha, ba, pch = 16, col = 2, cex = 1.2)
    } else if(type == "l") {
      matlines(tpha, ba, lwd = 2, col = 2)
    } else if(type == "b") {
      matlines(tpha, ba, lwd = 2, col = 2)
      matpoints(tpha, ba, pch = 16, col = 2, cex = 1.2)
    }
    if( is.vector(label) && !(label == "") ){
      text( tpha+4, ba, label, cex=0.75)
    }
  }
  
  
  invisible()
}

"qmdm"<-function(dm = 0, ba, tpha,a=c(-0.02053,0.02707,0.00199), lim=c(100,1100), adj=1, amd.coef=c(-0.6579,0.973))
{
  #  
  # Routine to calculate the metric Quadratic mean diameter from basal area and tree
  # per acre or return trees per acre and basal area from a given quadratic 
  # mean diameter.
  #
  # by David Larsen, Copyright June 11, 1993
  #
  if(dm == 0) {
    if( length(tpha[tpha<1]) != 0 ){
      tpha[tpha<1]=0.1
      qmd = 0.0
    }else{
      qmd = 200 * sqrt((ba/tpha)/pi) 
      qmd[is.na(qmd)]=0.0
    }
    return( qmd )
  }
  else { 
    ba <- vector(length=2)
    tpa <- vector(length=2)
    tpha <- vector(length=2)
    #dia <- dm/2.54
    amd <- amd.coef[1] + amd.coef[2] * dm
    number <- round(adj*(lim[2]/(a[1] + a[2] * amd + a[3] * dm^2)))
    minnum <- round(adj*(lim[1]/(a[1] + a[2] * amd + a[3] * dm^2)))
    for(i in minnum:number) {
      ba[i] <- pi * (dm/200)^2 * i 
      #ba[i] <- 0.00007854 * (dm)^2 * i / 2.0
      tpha[i] <- i
    }
    ba = ba 
    
    list(x = tpha, y = ba)
  }
}

"stocking.metric" <- function( dm=c(20,25,30,35,40,45,50,55,60), percent=100,a=c(-0.02053,0.02707,0.00199),b=c(0.070860,0.032680,0.003766), b.line=F, adj=1, amd.coef=c(-0.6579,0.973), qmd.out=F )
{
  #
  #  Routine to calculate the percent stocking line on a Gingrich diagram
  #
  #  by David Larsen, Copyright June 11, 1993
  #
  #dia = dm / 2.54
  number <- vector(length=length(dm))
  ba <- vector(length=length(dm))
  if( b.line ){
    for( i in 1:length(dm)){
      number[i] <- adj*(percent*10)/(b[1]+b[2]*dm[i]+b[3]*dm[i]^2)
      ba[i] <- pi /(4*10000) * (dm[i])^2 * number[i]
      #ba[i] <- 0.00007854 * (dm[i])^2 * number[i] /2.0 
    }
  }else{
    for( i in 1:length(dm)){
      amd <- amd.coef[1]+amd.coef[2]*dm[i]
      number[i] <- adj*(percent*10)/(a[1]+a[2]*amd+a[3]*dm[i]^2)
      ba[i] <- pi /(4*10000)*(dm[i])^2 * number[i]
      #ba[i] <- 0.00007854 * (dm[i])^2 * number[i] /2.0
    }
  }
  if( qmd.out == T ){
    list( x=number, y=sqrt(ba/number/0.00007854) )
  }else{
    #number = number 
    #ba = ba 
    list( x=number, y=ba )
  }
}



################################################################################
################################################################################
################################################################################


"plot.gingrich.imperial" <- function( dia=c(7,8,10,12,14,16,18,20,22),tpa=0,ba=0,ba.rng=c(0,160), tpa.rng=c(0,450),main="" , type = "p",a=c(-0.0507,0.1698,0.0317),b=c(0.0175,0.205,0.060),adj=1.0,color=F, label="",amd.coef=c(-0.259,0.973),power=F)
{
  #
  #  Routine to plot a Gingrich stocking diagram imperial units
  #    requires the "qmd.imperial" and "stocking.imperial" routines
  #
  #  by David Larsen, Copyright June 11, 1993
  #
  #  oldpar<-par();
  par(pty="s",lab=c(16,16,18))
  labels<-as.character(dia)
  matplot(tpa,ba,type="n",xlim=tpa.rng,ylim=ba.rng,
          xlab="Density (trees/acre)", ylab="Basal Area (sq. ft./acre)",main=main)
  #axis(1, tck=1, ,lty=3, col="black")
  #axis(2, tck=1, ,lty=3, col="black")
  par(lty=1,col="black")
  box()
  
  over<-stocking.imperial(dia=dia,b=b,a=a,percent=100,amd.coef=amd.coef)
  back<-stocking.imperial(dia=rev(dia),b=b,a=a,percent=110,amd.coef=amd.coef)
  under<-stocking.imperial(dia=dia,b=b,a=a,c.line=T,amd.coef=amd.coef)
  full<-stocking.imperial(dia=rev(dia),b=b,a=a,b.line=T,amd.coef=amd.coef)
  wood<-stocking.imperial(dia=dia,b=b,a=a,percent=30,amd.coef=amd.coef)
  cwood<-stocking.imperial(dia=dia,b=b,a=a,percent=90,amd.coef=amd.coef)
  
  low<-stocking.imperial(dia=dia, b=b, a=a, percent=30, amd.coef=amd.coef)
  high<-stocking.imperial(dia=dia, b=b, a=a, percent=75, amd.coef=amd.coef)
  
  under$x<-append(under$x,full$x)
  under$y<-append(under$y,full$y)
  wood$x<-append(wood$x,full$x)
  wood$y<-append(wood$y,full$y)
  cwood$x<-append(cwood$x,full$x)
  cwood$y<-append(cwood$y,full$y)
  full$x<-append(full$x,over$x)
  full$y<-append(full$y,over$y)
  over$x<-append(over$x,back$x)
  over$y<-append(over$y,back$y)

  target_x <- c(low$x, rev(high$x))
  target_y <- c(low$y, rev(high$y))
  
  if( color ){
    #   polygon(over,col="forestgreen")
    #   polygon(full,col="forestgreen")
    #   polygon(wood,col="forestgreen")
    #   polygon(cwood,col="forestgreen")
    #   polygon(under,col=5)
       polygon(target_x, target_y, col=5)
  }
  for( i in 1:(length(dia)) ){
    lines( qmd.imperial( dia[i],adj=adj,a=a,amd.coef=amd.coef ))
  }
  for( i in seq(10,110,10) ){
    lines( stocking.imperial(dia=dia,b=b,a=a, percent=i,adj=adj,amd.coef=amd.coef ))
    labs<-stocking.imperial(dia=dia[1],b=b,a=a, percent=i, adj=adj,amd.coef=amd.coef)
    text( labs$x+20,labs$y, paste(as.character(i),"%"),cex=0.75)
  }
  labs<-stocking.imperial(dia=dia,b=b,a=a,percent=115,adj=adj,amd.coef=amd.coef)
  text( labs$x,labs$y,labels,cex=0.75)
  labs<-stocking.imperial(dia=dia,b=b,a=a,adj=adj,amd.coef=amd.coef)
  text( labs$x[length(dia)]-25, labs$y[length(dia)],"A-line")
  lines(stocking.imperial(dia=dia,b=b,a=a,percent=100,amd.coef=amd.coef),lwd=2)
  #labs<-stocking.imperial(dia=dia,b=b,a=a, b.line=T,amd.coef=amd.coef)
  #text( labs$x[length(dia)]-25, labs$y[length(dia)],"B-line")
  #lines(stocking.imperial(dia=dia,b=b,a=a,b.line=T),lwd=2,amd.coef=amd.coef)
  labs<-stocking.imperial(dia=dia,b=b,a=a, b.line=T,amd.coef=amd.coef)
  text( labs$x[length(dia)]-25, labs$y[length(dia)],"B-line")
  lines(stocking.imperial(dia=dia,b=b,a=a,b.line=T,amd.coef=amd.coef),lwd=2)
  labs<-stocking.imperial(dia=dia,b=b,a=a, c.line=T,amd.coef=amd.coef)
  text( labs$x[length(dia)]-32, labs$y[length(dia)]+5,"C-line")
  lines(stocking.imperial(dia=dia,b=b,a=a,diagrow=seq(4.6,10.5,length.out=9),c.line=T,amd.coef=amd.coef),lwd=2)
  text( tpa.rng[2]/2, ba.rng[2]*.95,"Quadratic Mean Diameter",adj=0)
  text( tpa.rng[2]/1.5, ba.rng[2]*.92,"(inches)",cex=0.7,adj=0)
  #  text( tpa.rng[2]/2, ba.rng[2]*.95,"Average Tree Diameter(in.)",adj=0)
  labs<-stocking.imperial(dia=dia[1],b=b,a=a, percent=50,adj=adj,amd.coef=amd.coef)
  text( labs$x+50, labs$y,"Percent Stocking",adj=0)
  #  text( labs$x+10, labs$y,"Crown Cover (Percent)",adj=0)
  if( (is.vector(tpa) || is.matrix(tpa)) ){
    if(type == "p") {
      matpoints(tpa, ba, pch = 16, col = 2, cex = 2)
    } else if(type == "l") {
      matlines(tpa, ba, lwd = 2, col = 2)
    } else if(type == "b") {
      matlines(tpa, ba, lwd = 2, col = 2)
      matpoints(tpa, ba, pch = 16, col = 2, cex = 1.2)
    }
    if(!is.null(label)){
      text(tpa + 15, ba - 2, labels = label, cex = 0.75)
    }
  }
  #  par(oldpar);
  invisible()
}

"qmd.imperial"<-
  function(dia = 0, ba, tpa, a=c(-0.0507,0.1698,0.0317), lim=c(100,1100), adj=1, amd.coef=c(-1.88128,0.9535))
  {
    #
    # Routine to calculate the Quadratic mean diameter from basal area and tree
    # per acre or return trees per acre and basal area from a given quadratic
    # mean diameter in imperial units.
    #
    # by David Larsen, Copyright June 11, 1993
    #
    if(dia == 0) {
      if( length(tpa[tpa<1]) != 0 ){
        tpa[tpa<1]=0.1
        qmd = 0.0
      }else{
        qmd = 24 * sqrt((ba/tpa)/pi)
        qmd[is.na(qmd)]=0.0
      }
      return( qmd )
    }
    else {
      ba <- vector(length=2)
      tpa <- vector(length=2)
      amd <- amd.coef[1] + amd.coef[2] * dia
      number <- round(adj*(lim[2]/(a[1] + a[2] * amd + a[3] * dia^2)))
      minnum <- round(adj*(lim[1]/(a[1] + a[2] * amd + a[3] * dia^2)))
      for(i in minnum:number) {
        ba[i] <- pi * (dia/24)^2 * i
        tpa[i] <- i
      }
      list(x = tpa, y = ba)
    }
  }

"stocking.imperial" <- function( dia=c(10,12,14,16,18,20,22,24,26), percent=100, c.line=F, diagrow=rep(4.9,9), a=c(-0.0507,0.1698,0.0317), b.line=F, b=c(0.175,0.205,0.060), adj=1, amd.coef=c(-1.88128,0.9535), qmd.out=F )
{
  #
  #  Routine to calculate the percent stocking line on a Gingrich diagram in imperial units
  #
  #  by David Larsen, Copyright June 11, 1993
  #
  number <- vector(length=length(dia))
  ba <- vector(length=length(dia))
  if( b.line ){
    for( i in 1:length(dia)){
      number[i] <- adj*(percent*10)/(b[1]+b[2]*dia[i]+b[3]*dia[i]^2)
      ba[i] <- pi * (dia[i]/24)^2 * number[i]
    }
  }else if( c.line ){
    for( i in 1:length(dia)){
      dia[i] <- dia[i]+diagrow[i]
      number[i] <- adj*(percent*10)/(a[1]+a[2]*dia[i]+a[3]*dia[i]^2)
      ba[i] <- 72.00 - 0.224*number[i] + 0.000254*number[i]^2
    }
  }else{
    for( i in 1:length(dia)){
      amd <- amd.coef[1]+amd.coef[2]*dia[i]
      number[i] <- adj*(percent*10)/(a[1]+a[2]*amd+a[3]*dia[i]^2)
      ba[i] <- pi * (dia[i]/24)^2 * number[i]
    }
  }
  if( qmd.out == T ){
    list( x=number, y=sqrt(ba/number/0.005454154) )
  }else{
    list( x=number, y=ba )
  }
}
