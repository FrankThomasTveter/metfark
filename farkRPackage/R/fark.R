#
###################### input data

# Comment arrays: attributes legends columns
#
getArguments <- function () {
   args = commandArgs(trailingOnly=TRUE);
   if (length(args)==0) {
      print ("At least one argument must be supplied (using defaults).");
     args[1] = "meps_syno_003d.table";
     args[2] = "output/meps_syno_003d/";
   } else if (length(args)==1) {
     # default output file
     args[2] = "output/";
   };
   return (args);
}

readComments <- function (filename,name) {
   print (paste("readComments Entering ",filename,name));
   con = file(filename, "r");
   while ( TRUE ) {
     line = readLines(con, n = 1);
     if ( length(line) == 0 ) { break;}
     if (substr(line,1,1) != "#") {break;}
     items <- strsplit(line,":")
     #print (line);
     if (items[1] == "# COMMAND") { # do nothing
     } else if (items[[1]][1] == paste("#",name)) {
       n=as.numeric(items[[1]][2]);
       nams <- c();
       vals <- c();
       for (i in 1:n) {
         line = readLines(con, n = 1);
         items <- strsplit(substr(line,2,1000000L),":")
         nam <- gsub(" ","",items[[1]][1],fixed = TRUE);
         val <- items[[1]][2];
         nams=c(nams,nam);
         vals=c(vals,val);
       };  
       output <- matrix(vals, nrow=n, ncol=1);
       rownames(output)<-nams;
     }
   }
   close(con);
   print ("readComment Done. ");
   print (output);
   return (output);
}

readData <- function (filename) {
    data <- read.table(filename,header=TRUE);
    return (data);
}

getFormat <- function(attributes) {
  return (toString(attributes["Format",1]));
}

getOrientation <- function(attributes) {
  return (identical(toString(attributes["Orientation",1]),"landscape"));
}

###################### output data

## function to open graphics output
openPlot <- function(filename,attributes) {
  makeDir(filename);
  format=getFormat(attributes);
  orientation=getOrientation(attributes);
  if (identical(format,"Jpeg")) {
   print(paste("Opening jpeg:",filename));
   jpeg(filename);
  } else if (identical(format,"png")) {
   print(paste("Opening png:",filename));
   png(filename);
  } else if (identical(format,"PDF")) {
   print(paste("Opening PDF:",filename));
   pdf(filename);
  } else if (identical(format,"PostScript")) {
   print(paste("Opening PostScript:",filename));
   postscript(filename,horizontal=orientation);
  } else {
   print("Unknown graphics type.");
   print(format);
  };
};

closePlot <- function() {
  dev.off();
};

###################### data processing

### "me","sde","rms","mae","cnt"
#getStats <- function(d) {
#   return (c(mean(d),sd(d),sqrt(mean(d^2)),mean(abs(d)),length(d)));
#}

norwegian <- function(data,idcol,ll) {
  s <- data[,idcol];
  if (missing(ll)) {
     output <- (s>=1000 & s < 2000);
  } else {
     output <- (s>=1000 & s < 2000 & ll);
  }
  if (sum(output)==0) {
      print ("No norwegian stations found. Stations:");
      print(unique(data[,idcol]));
  }
  return (output);
}

below <- function(data,altcol,thres,ll) {
  g <- as.numeric(data[,altcol]);
  t <- as.numeric(thres);
  if (missing(ll)) {
     output <- (g <= t);
  } else {
     output <- (g <= t & ll);
  }
  return (output);
}

above <- function(data,altcol,thres,ll) {
  g <- as.numeric(data[,altcol]);
  t <- as.numeric(thres);
  if (missing(ll)) {
     output <- (g > t);
  } else {
     output <- (g > t & ll);
  }
  return (output);
}

# thinned data
thinned <- function (data,ll) { 
  set.seed(1);
  thin_sample=(1:nrow(data));
  if (missing(ll)) {
    thin_data <- thin_sample;
  } else {
    thin_data <- thin_sample[ll];
  };
  thin_n = min(c(500,length(thin_data)));
  print (paste("Thinning:",nrow(data),"->",length(thin_data)," -> ",thin_n));
  thin_extract <- sample(thin_data,thin_n, replace=FALSE)
  return (thin_extract);
}
#

getDTG <- function (epoch) { # expects seconds - not milliseconds...
#   return(as.POSIXct(epoch, origin="1970-01-01"));#"2012-11-04 22:32:00 CST"
   return(as.Date(as.POSIXct(epoch, origin="1970-01-01"))); # "2012-11-05" 
}

getMinMaxDTGs <- function (data,ll,col) {
    mi <- min(data[ll,col]);
    ma <- max(data[ll,col]);
    mid= toString(getDTG(mi));
    mad= toString(getDTG(ma));
    print (paste("Found date:",mid));
    return (c(mid,mad,prettyTime(ma-mi)));
}

## function to make sure output directory exists...
makeDir <- function(path) {
   dd = dirname(path);
   dir.create(dd, recursive = TRUE, mode = "0777", showWarnings = FALSE);
}

pretty <- function(num) {
    return (format(num,big.mark=",", trim=TRUE));
}
prettyTime <- function(num) {
    num <- num/86400.0;
    dd <- round(num,1);
    return (paste0(format(dd,big.mark=",", trim=TRUE),"d"));
}

getAxisDates <- function (dtgs) {
    req <- 5;            # requested number of dates 
    cc <- length(dtgs);  # length
    ss=ceiling(cc/req);  # step
    xx <- rep(NA,cc);    # x-values
    dd <- rep(NA,cc);    # labels
    ot <- as.Date(dtgs[[1]] - 1,origin='1970-01-01'); # day before first
    tt <- as.Date(dtgs,origin='1970-01-01'); # full labels
    ll=nchar(toString(tt[[1]]));
    #print(paste("Characters=",ll,"(",toString(tt[[1]]),")"));
    for (l in 1:ll) { # loop over increasing strin length
        #print(paste("Processing=",l,"(",ll,")"));
        ts <- substr(tt,1,l); # current day substring
        os <- substr(ot,1,l); # old day substring
        for (i in 1:cc) { # check
            ns=ts[[i]];
            if (os != ns) {
                #print(paste("New candidate=",i," Str=",ns));
                ff=FALSE;
                for (j in max(1,i-ss+1):min(cc,i+ss-1)) {
                    if (! is.na(xx[[j]])) {
                        #print(paste("Found=",i," J=",j," dtg=",xx[[j]]));
                        ff=TRUE;
                    };
                };
                if (! ff) {
                    xx[[i]]=dtgs[[i]];
                    dd[[i]]=toString(as.Date(dtgs[[i]],origin='1970-01-01'));
                }
            }
            os=ns;
        }
    }
    #print(paste("Xvalue=",xx," Dtg=",dd));
    return (list(xx,dd));
}

fadeColor <- function(n) { # fade to "white"
    return (colorRampPalette(c(palette()[[n]],"white"))(3)[[2]]);
};

transColor <- function(n) { # transparent color
    alpha=0.8;
    col <- colorRampPalette(c(palette()[[n]],"white"))(3)[[2]];
    return (paste(col, sprintf("%x", ceiling(255*alpha)), sep=""));
};
    
thin <- function(x, npoints){
  #Create empty vector for output
  inds <- vector(mode="numeric")
  #Create distance matrix
  this.dist <- as.matrix(dist(x, upper=TRUE))
  #Draw first index at random
  inds <- c(inds, as.integer(1));
  #Get second index from maximally distant point from first one
  #Necessary because apply needs at least two columns or it'll barf
  #in the next bit
  inds <- c(inds, which.max(this.dist[,inds]))
  while(length(inds) < npoints){
    #For each point, find its distance to the closest point that's already been selected
    min.dists <- apply(this.dist[,inds], 1, min)
    #Select the point that is furthest from everything we've already selected
    this.ind <- which.max(min.dists)
    #Get rid of ties, if they exist
    if(length(this.ind) > 1){
      print("Breaking tie...")
      this.ind <- this.ind[1]
    }
    inds <- c(inds, this.ind)
  }
  inds <- sort(inds);
  return(x[inds])
}

# pch = points type
# lty = line type

###################### scatter plot (... -> ["label",selection]...)

scatterPlot <- function(filename,attr,leg,data,title,setcol,refcol,timecol,
                        idcol,altcol,xcol,ycol,xlab,ylab,...) {
    lst=list(...);
    inp <- matrix(lst,ncol=2,byrow=TRUE);
    ilab <- inp[,1]; # labels
    isel <- inp[,2]; # selection
    x <- data[,xcol];
    y <- data[,ycol];
    valid <- (!is.na(x) & !is.na(y));
    print (paste("Number of stations:          ",sum(valid)));
    dtgs <- getMinMaxDTGs(data,valid,timecol);
    dset <- unique(data[,setcol]);
    legs=c();
    cols=c();
    pchs=c();
    stats=c();
    if (length(isel) == 0 ) {isel <- list(valid);ilab <- list("");};
    if (sum(valid)>1) {
        openPlot(filename,attr);
        xr <- range(x[valid]);
        yr <- range(y[valid]);
        rr=range(union(xr,yr));
        plot(xr,yr,type="n",xlab=xlab,ylab=ylab);#,tck = 0.0
        title(paste(paste0("'",title,"'"),"(model vs observation)"));
        for (dd in dset) {  # loop over datasets
            for (ii in 1:length(isel)) { # loop over selections
                l <- ilab[[ii]];
                dsel <- (isel[[ii]] & data[,setcol] == dd & valid);
                d    <- (y[dsel]-x[dsel]);
                dme  <- round(mean(d),2);
                dsde <- round(sd(d),2) ;
                drms <- round(sqrt(mean(d^2)),2);
                dmae <- round(mean(abs(d)),2);
                dcnt <- pretty(sum(dsel));
                stat <- paste0("me=",dme,",sde=",dsde,",mae=",dmae,",cnt=",dcnt);
                print (paste0("Set=",dd," count=",pretty(sum(dsel))," (",l,")"));
                if (sum(dsel) > 0) {
                    td <- thinned(data,dsel);
                    lines(x[td],y[td],type="p",pch=dd,col=ii);
                    if (l=="") {
                        legs=append(legs,paste0(leg[dd]));
                    } else {
                        legs=append(legs,paste0(leg[dd]," (",l,")"));
                    };
                    cols=append(cols,ii);
                    pchs=append(pchs,dd);
                    stats=append(stats,stat);
                };
            };
        };
        lines(rr,rr,type="l",lty=2,col=gray(0.75));
        info=paste("From",toString(dtgs[1]),
                   "to",as.Date(dtgs[2],origin="1970-01-01"),paste0("(",dtgs[3],")"));
        legend("bottomright",legend=info,bty="n",cex=0.75*par("cex"));
        if (length(legs)>0) {
            print ("Adding legend.");
            legend("topleft",legend=legs,col=cols,pch=pchs,
                   bty="n",cex=0.75*par("cex"));
            legend("topright",legend=stats,bty="n",cex=0.75*par("cex"));
        };
        closePlot();
    } else {
        print (paste("$$$ No data for ",filename));
    }
};

###################### score plot

scorePlot <- function(filename,attr,leg,data,title,setcol,refcol,timecol,
                      idcol,altcol,modcol,obscol,lab,...) {
    lst=list(...); # "label1",selection1,"label2",selection2...
    inp <- matrix(lst,ncol=2,byrow=TRUE);
    ilab <- inp[,1]; # labels
    isel <- inp[,2]; # selection
    mod <- data[,modcol];
    obs <- data[,obscol];
    valid <- (!is.na(mod) & !is.na(obs));
    print (paste("Number of stations:          ",sum(valid)));
    if (sum(valid)>1) {
        if (length(isel) == 0 ) {isel <- list(valid);ilab <- list("");};
        dtgs <- getMinMaxDTGs(data,valid,timecol);
        dset <- unique(data[,setcol]);
        ;# make data statistics...
        lead  <- round((data[,timecol]-data[,refcol])/3600.0);
        leadu <- unique(lead[valid]);
        xl    <- list();
        ymel  <- list();
        ysdel <- list();
        yrmsl <- list();
        ymael <- list();
        ycntl <- list();
        dl    <- list();
        il    <- list();
        xr    <- c();
        yr    <- c();
        statl <- list();
        for (dd in dset) {                # loop over datasets
            for (ii in 1:length(isel)) {  # loop over selections
                x    <- c();
                yme  <- c();
                ysde <- c();
                yrms <- c();
                ymae <- c();
                ycnt <- 0;
                # overall statistics
                dsel <-  (isel[[ii]] & data[,setcol] == dd & valid );
                d    <- (mod[dsel]-obs[dsel]);
                dme  <- round(mean(d),2);
                dsde <- round(sd(d),2) ;
                drms <- round(sqrt(mean(d^2)),2);
                dmae <- round(mean(abs(d)),2);
                dcnt <- pretty(sum(dsel));
                stat <- paste0("me=",dme,",sde=",dsde,",mae=",dmae,",cnt=",dcnt);
                for (ll in sort(leadu)) {    # loop over lead times
                    dsel <- (isel[[ii]] & data[,setcol] == dd & valid & lead == ll );
                    if (sum(dsel)>2) {
                        x    <- append(x,ll);
                        d    <- (mod[dsel]-obs[dsel]);
                        yme  <- append(yme,  mean(d) );
                        ysde <- append(ysde, sd(d) );
                        yrms <- append(yrms, sqrt(mean(d^2)) );
                        ymae <- append(ymae, mean(abs(d)) );
                        ycnt <- ycnt + length(d);
                    };
                };
                if (length(x)>0) {
                    ;### store all values... and handle range 
                    xl    <- append(xl,list(x));
                    ymel  <- append(ymel,list(yme));
                    ysdel <- append(ysdel,list(ysde));
                    yrmsl <- append(yrmsl,list(yrms));
                    ymael <- append(ymael,list(ymae));
                    ycntl <- append(ycntl,list(ycnt));
                    dl    <- append(dl,list(dd));
                    il    <- append(il,list(ii));
                    xr    <- range( append(xr,x));
                    yr    <- range( append(yr,yme));
                    yr    <- range( append(yr,ysde));
                    statl <- append(statl,list(stat));
                };
            };
        };
        ;# make plot
        if (length(xr)>0) { # we have data
            openPlot(filename,attr);
            plot(xr,yr,type="n",xlab=paste("Lead time (h)"),ylab=lab);#,tck = 0.0
            title(paste(paste0("'",title,"'"),"SDE+BIAS (model-observation)"));
            legs=c();
            cols=c();
            ltys=c();
            lwds=c();
            stats=c();
            for (jj in 1:length(dl)) {
                x    <- xl[[jj]];
                yme  <- ymel[[jj]];
                ysde <- ysdel[[jj]];
                yrms <- yrmsl[[jj]];
                ymae <- ymael[[jj]];
                ycnt <- ycntl[[jj]];
                dd   <- dl[[jj]];
                ii   <- il[[jj]];
                l    <- ilab[[ii]];
                stat <- statl[[jj]];
                lines(x,ysde,type="l",lty=dd,col=ii,lwd=3);        # stde
                lines(x,yme,type="l",lty=dd,col=fadeColor(ii),lwd=3); # bias
                if (l=="") {
                    legs=append(legs,paste0(leg[dd]));
                } else {
                    legs=append(legs,paste0(leg[dd]," (",l,")"));
                }; 
                cols=append(cols,ii);
                ltys=append(ltys,dd);
                lwds=append(lwds,3); # 3/0.75
                stats=append(stats,stat);
            };
            lines(xr,c(0.,0.),type="l",lty=2,col=gray(0.75));
            info=paste("From",toString(dtgs[1]),
                       "to",as.Date(dtgs[2],origin="1970-01-01"),paste0("(",dtgs[3],")"));
            legend("bottomright",legend=info,bty="n",cex=0.75*par("cex"));
            if (length(legs)>0) {
                print ("Adding legend.");
                legend("topleft",legend=legs,col=cols,lty=ltys,lwd=lwds,
                       bty="n",cex=0.75*par("cex"));
                legend("topright",legend=stats,bty="n",cex=0.75*par("cex"));
            };
            closePlot();
        } else {
            print (paste("$$$ No plot data for ",filename));
        }
    } else {
        print (paste("$$$ No valid data for ",filename));
    }
};

###################### series plot
    
seriesPlot <- function(filename,attr,leg,data,title,setcol,refcol,timecol,
                       idcol,altcol,modcol,obscol,lab,...) {
    lst=list(...); # "label1",selection1,"label2",selection2...
    inp <- matrix(lst,ncol=2,byrow=TRUE);
    ilab <- inp[,1]; # labels
    isel <- inp[,2]; # selection
    mod <- data[,modcol];
    obs <- data[,obscol];
    valid <- (!is.na(mod) & !is.na(obs));
    print (paste("Number of stations:          ",sum(valid)));
    if (sum(valid)>1) {
        if (length(isel) == 0 ) {isel <- list(valid);ilab <- list("");};
        dtgs <- getMinMaxDTGs(data,valid,timecol);
        dset <- unique(data[,setcol]);
        ;# make data statistics...
        dtg   <- floor((data[,timecol]/86400.0));
        dtgu  <- unique(dtg[valid]);
        xdtg  <- getAxisDates(dtgu);
        xl    <- list();
        ymel  <- list();
        ysdel <- list();
        yrmsl <- list();
        ymael <- list();
        ycntl <- list();
        dl    <- list();
        il    <- list();
        xr    <- c();
        yr    <- c();
        statl <- list();
        for (dd in dset) {                # loop over datasets
            for (ii in 1:length(isel)) {  # loop over selections
                x    <- c();
                yme  <- c();
                ysde <- c();
                yrms <- c();
                ymae <- c();
                ycnt <- 0;
                # overall statistics
                dsel <-  (isel[[ii]] & data[,setcol] == dd & valid );
                d    <- (mod[dsel]-obs[dsel]);
                dme  <- round(mean(d),2);
                dsde <- round(sd(d),2) ;
                drms <- round(sqrt(mean(d^2)),2);
                dmae <- round(mean(abs(d)),2);
                dcnt <- pretty(sum(dsel));
                stat <- paste0("me=",dme,",sde=",dsde,",mae=",dmae,",cnt=",dcnt);
                for (ll in sort(dtgu)) {    # loop over times
                    dsel <- (isel[[ii]] & data[,setcol] == dd & valid & dtg == ll );
                    if (sum(dsel)>2) {
                        x    <- append(x,ll);
                        d    <- (mod[dsel]-obs[dsel]);
                        yme  <- append(yme,  mean(d) );
                        ysde <- append(ysde, sd(d) );
                        yrms <- append(yrms, sqrt(mean(d^2)) );
                        ymae <- append(ymae, mean(abs(d)) );
                        ycnt <- ycnt + length(d);
                    };
                };
                if (length(x)>0) {
                    ;### store all values... and handle range 
                    xl    <- append(xl,list(x));
                    ymel  <- append(ymel,list(yme));
                    ysdel <- append(ysdel,list(ysde));
                    yrmsl <- append(yrmsl,list(yrms));
                    ymael <- append(ymael,list(ymae));
                    ycntl <- append(ycntl,list(ycnt));
                    dl    <- append(dl,list(dd));
                    il    <- append(il,list(ii));
                    xr    <- range( append(xr,x));
                    yr    <- range( append(yr,yme));
                    yr    <- range( append(yr,ysde));
                    statl <- append(statl,list(stat));
                };
            };
        };
        ;# make plot
        if (length(xr)>0) { # we have data
            ###xr <- as.POSIXct(xr, origin="1970-01-01");
            openPlot(filename,attr);
            plot(xr,yr,type="n",ylab=lab,xlab="time",xaxt="n");
            axis(1,at=xdtg[[1]],labels=xdtg[[2]], cex.axis = .75*par("cex"));
            title(paste(paste0("'",title,"'"),"SDE+BIAS (model-observation)"));
            legs=c();
            cols=c();
            ltys=c();
            lwds=c();
            stats=c();
            for (jj in 1:length(dl)) {
                x    <- xl[[jj]];
                yme  <- ymel[[jj]];
                ysde <- ysdel[[jj]];
                yrms <- yrmsl[[jj]];
                ymae <- ymael[[jj]];
                ycnt <- ycntl[[jj]];
                dd   <- dl[[jj]];
                ii   <- il[[jj]];
                l    <- ilab[[ii]];
                stat <- statl[[jj]];
                ###x <- as.POSIXct(x, origin="1970-01-01");
                lines(spline(x,ysde),type="l",lty=dd,col=ii,lwd=3);        # stde
                lines(spline(x,yme),type="l",lty=dd,col=fadeColor(ii),lwd=3); # bias
                if (l=="") {
                    legs=append(legs,paste0(leg[dd]));
                } else {
                    legs=append(legs,paste0(leg[dd]," (",l,")"));
                }; 
                cols=append(cols,ii);
                ltys=append(ltys,dd);
                lwds=append(lwds,3); # 3/0.75
                stats=append(stats,stat);
            };
            lines(xr,c(0.,0.),type="l",lty=2,col=gray(0.75));
            info=paste("From",toString(dtgs[1]),
                       "to",as.Date(dtgs[2],origin="1970-01-01"),paste0("(",dtgs[3],")"));
            legend("bottomright",legend=info,bty="n",cex=0.75*par("cex"));
            if (length(legs)>0) {
                print ("Adding legend.");
                legend("topleft",legend=legs,col=cols,lty=ltys,lwd=lwds,
                       bty="n",cex=0.75*par("cex"));
                legend("topright",legend=stats,bty="n",cex=0.75*par("cex"));
            };
            closePlot();
        } else {
            print (paste("$$$ No plot data for ",filename));
        }
    } else {
        print (paste("$$$ No valid data for ",filename));
    }
};

###################### historgram plot

histPlot <- function(filename,attr,leg,data,title,setcol,refcol,timecol,
                        idcol,altcol,modcol,obscol,lab,...) {
    lst=list(...);
    inp <- matrix(lst,ncol=2,byrow=TRUE);
    ilab <- inp[,1]; # labels
    isel <- inp[,2]; # selection
    m <- data[,modcol];
    o <- data[,obscol];
    valid <- (!is.na(m) & !is.na(o));
    print (paste("Number of stations:          ",sum(valid)));
    dtgs <- getMinMaxDTGs(data,valid,timecol);
    dset <- unique(data[,setcol]);
    xl  <- list();
    yl  <- list();
    dl  <- list();
    il  <- list();
    xr  <- c();
    yr  <- c();
    legs = c();
    cols = c();
    ltys = c();
    lwds = c();
    stats= c();
    if (length(isel) == 0 ) {isel <- list(valid);ilab <- list("");};
    if (sum(valid)>1) {
        h <- (m[valid]-o[valid]);
        xlim <- range(h);
        p0 <- hist(h,nclass=200,plot=FALSE);  # plot(p0$mids,p0$density)
        #title();
        for (dd in dset) {  # loop over datasets
            for (ii in 1:length(isel)) { # loop over selections
                l <- ilab[[ii]];
                dsel <- (isel[[ii]] & data[,setcol] == dd & valid);
                print (paste0("Set=",dd," count=",pretty(sum(dsel))," (",l,")"));
                if (sum(dsel) > 0) {
                    d <- m[dsel]-o[dsel];
                    pp <- hist(d,nclass=50,plot=FALSE);
                    x <- pp$mids;
                    y <- pp$density;
                    if (length(x)>0) {
                        ;### store all values... and handle range 
                        xl    <- append(xl,list(x));
                        yl    <- append(yl,list(y));
                        dl    <- append(dl,list(dd));
                        il    <- append(il,list(ii));
                        xr    <- range( append(xr,x));
                        yr    <- range( append(yr,y));
                        dme  <- round(mean(d),2);
                        dsde <- round(sd(d),2) ;
                        drms <- round(sqrt(mean(d^2)),2);
                        dmae <- round(mean(abs(d)),2);
                        dcnt <- pretty(sum(dsel));
                        stat <- paste0("me=",dme,",sde=",dsde,",mae=",dmae,",cnt=",dcnt);
                        if (l=="") {
                            legs=append(legs,paste0(leg[dd]));
                        } else {
                            legs=append(legs,paste0(leg[dd]," (",l,")"));
                        };
                        cols = append(cols,ii);
                        ltys = append(ltys,dd);
                        lwds = append(lwds,3); # 3/0.75
                        stats= append(stats,stat);
                    };
                };
            };
        };
        if (length(xr)>0) { # we have data
            openPlot(filename,attr);
            plot(xr,yr,type="n",xlim=xlim,
                 main=paste(paste0("'",title,"'"),"(model-observation)"),
                 xlab=label,
                 ylab="Density");
            for (jj in 1:length(xl)) {
                x    <- xl[[jj]];
                y    <- yl[[jj]];
                dd   <- dl[[jj]];
                ii   <- il[[jj]];
                lines(spline(x,y),type="l",lty=dd,col=ii,lwd=3);
            };
            lines(c(0.,0.),yr,type="l",lty=2,col=gray(0.75));
            info=paste("From",toString(dtgs[1]),
                       "to",as.Date(dtgs[2],origin="1970-01-01"),paste0("(",dtgs[3],")"));
            legend("bottomright",legend=info,bty="n",cex=0.75*par("cex"));
            if (length(legs)>0) {
                print ("Adding legend.");
                legend("topleft",legend=legs,col=cols,lty=ltys,
                       lwd=lwds,bty="n",cex=0.75*par("cex"));
                legend("topright",legend=stats,bty="n",cex=0.75*par("cex"));
            };
            closePlot();
        };
    } else {
        print (paste("$$$ No data for ",filename));
    }
};

###################### vertical profile plot

profilePlot <- function(filename,attr,leg,data,title,setcol,refcol,timecol,
                       idcol,hgtlab,hgtcol,hgtdir,modcol,obscol,lab,...) {
    nhgt <- 10; # number of height bins
    lst=list(...); # "label1",selection1,"label2",selection2...
    inp <- matrix(lst,ncol=2,byrow=TRUE);
    ilab <- inp[,1]; # labels
    isel <- inp[,2]; # selection
    mod <- data[,modcol];
    obs <- data[,obscol];
    valid <- (!is.na(mod) & !is.na(obs));
    print (paste("Number of stations:          ",sum(valid)));
    if (sum(valid)>1) {
        if (length(isel) == 0 ) {isel <- list(valid);ilab <- list("");};
        dtgs <- getMinMaxDTGs(data,valid,timecol);
        dset <- unique(data[,setcol]);
        ;# make data statistics...
        hgt   <- (data[,hgtcol]);
        #print (unique(hgt));
        bhgt  <- quantile(hgt,probs = seq(0, 1, by = 1.0/nhgt),na.rm=TRUE)
        qhgt  <- findInterval(hgt,bhgt);
        ;# initialise data arrays...
        yl    <- list();
        xmel  <- list();
        xsdel <- list();
        xrmsl <- list();
        xmael <- list();
        xcntl <- list();
        dl    <- list();
        il    <- list();
        xr    <- c();
        yr    <- c();
        statl <- list();
        for (dd in dset) {                # loop over datasets
            for (ii in 1:length(isel)) {  # loop over selections
                y    <- c();
                xme  <- c();
                xsde <- c();
                xrms <- c();
                xmae <- c();
                xcnt <- 0;
                ;# overall statistics
                dsel <-  (isel[[ii]] & data[,setcol] == dd & valid );
                d    <- (mod[dsel]-obs[dsel]);
                dme  <- round(mean(d),2);
                dsde <- round(sd(d),2) ;
                drms <- round(sqrt(mean(d^2)),2);
                dmae <- round(mean(abs(d)),2);
                dcnt <- pretty(sum(dsel));
                stat <- paste0("me=",dme,",sde=",dsde,",mae=",dmae,",cnt=",dcnt);
                #print(paste("Sel:",ii," stat:",stat));
                for (ll in 1:nhgt) {    # loop over heights
                    dsel <- (isel[[ii]] & data[,setcol] == dd & valid & qhgt == ll );
                    if (sum(dsel)>2) {
                        y    <- append(y,mean(hgt[dsel]));
                        d    <- (mod[dsel]-obs[dsel]);
                        xme  <- append(xme,  mean(d) );
                        xsde <- append(xsde, sd(d) );
                        xrms <- append(xrms, sqrt(mean(d^2)) );
                        xmae <- append(xmae, mean(abs(d)) );
                        xcnt <- xcnt + length(d);
                    };
                };
                if (length(y)>0) {
                    ;### store all values... and handle range 
                    yl    <- append(yl,list(y));
                    xmel  <- append(xmel,list(xme));
                    xsdel <- append(xsdel,list(xsde));
                    xrmsl <- append(xrmsl,list(xrms));
                    xmael <- append(xmael,list(xmae));
                    xcntl <- append(xcntl,list(xcnt));
                    dl    <- append(dl,list(dd));
                    il    <- append(il,list(ii));
                    yr    <- range( append(yr,y));
                    xr    <- range( append(xr,xme));
                    xr    <- range( append(xr,xsde));
                    statl <- append(statl,list(stat));
                };
            };
        };
        ;# make plot
        if (length(yr)>0) { # we have data
            ###xr <- as.POSIXct(xr, origin="1970-01-01");
            openPlot(filename,attr);
            if (hgtdir == "descending") {
                plot(xr,yr,type="n",xlab=lab,ylab=hgtlab,ylim=rev(yr));
            } else {
                plot(xr,yr,type="n",xlab=lab,ylab=hgtlab,ylim=yr);
            };
            title(paste(paste0("'",title,"'"),"SDE+BIAS (model-observation)"));
            legs=c();
            cols=c();
            ltys=c();
            lwds=c();
            stats= c();
            #print(paste("Selections:",length(dl)));
            for (jj in 1:length(dl)) {
                y    <- yl[[jj]];
                xme  <- xmel[[jj]];
                xsde <- xsdel[[jj]];
                xrms <- xrmsl[[jj]];
                xmae <- xmael[[jj]];
                xcnt <- xcntl[[jj]];
                dd   <- dl[[jj]];
                ii   <- il[[jj]];
                l    <- ilab[[ii]];
                stat <- statl[[jj]];
                ###x <- as.POSIXct(x, origin="1970-01-01");
                #print(paste("Plotting:",jj,"X:",xsde,"Y:",y));
                lines(xsde,y,type="l",lty=dd,col=ii,lwd=3);        # stde
                lines(xme,y,type="l",lty=dd,col=fadeColor(ii),lwd=3); # bias
                if (l=="") {
                    legs=append(legs,paste0(leg[dd]));
                } else {
                    legs=append(legs,paste0(leg[dd]," (",l,")"));
                }; 
                cols=append(cols,ii);
                ltys=append(ltys,dd);
                lwds=append(lwds,3); # 3/0.75
                stats=append(stats,stat);
            };
            lines(c(0.,0.),yr,type="l",lty=2,col=gray(0.75));
            info=paste("From",toString(dtgs[1]),
                       "to",as.Date(dtgs[2],origin="1970-01-01"),paste0("(",dtgs[3],")"));
            legend("bottomright",legend=info,bty="n",cex=0.75*par("cex"));
            if (length(legs)>0) {
                print ("Adding legend.");
                legend("topleft",legend=legs,col=cols,lty=ltys,lwd=lwds,
                       bty="n",cex=0.75*par("cex"));
                legend("topright",legend=stats,bty="n",cex=0.75*par("cex"));
            };
            closePlot();
        } else {
            print (paste("$$$ No plot data for ",filename));
        }
    } else {
        print (paste("$$$ No valid data for ",filename));
    }
};
