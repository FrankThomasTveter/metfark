#
###################### input data

# Comment arrays: attributes legends columns
#
getArguments <- function () {
   args = commandArgs(trailingOnly=TRUE);
   if (length(args)==0) {
      print ("At least one argument must be supplied (using defaults).");
     args[1] = "test.table";
     args[2] = "output/test/";
   } else if (length(args)==1) {
     # default output file
     args[2] = "output/";
   };
   return (args);
}

readComments <- function (filename,name) {
   print (paste("readComments Entering ",filename,name));
   con = file(filename, "r");
   output <- matrix(, nrow = 0, ncol = 0);
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
       rm(nams);rm(vals);
     }
   }
   close(con);
   print ("readComment Done. ");
   print (output);
   return (output);
}

grepFile <- function (filename,pattern,suffix) {
    if (missing(suffix)) {suffix=pattern;};
    fileout=paste0(filename,suffix);
    print(paste("Pattern:",pattern));
    cmd = paste("grep ",paste0("'",pattern,"'"),filename,">",fileout);
    print(paste("Processing:",cmd));
    system(cmd);
    return(fileout);
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

# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

getVal <- function (x,y) {
    if (x %in% rownames(y)) {
        return(y[x,]);
    } else {
        return("");
    }
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

# return the data with only overlapping data present in all sets...
overlap <- function (raw,setcol,selcols,sel) {
    trgcol = "index";
    if (missing(sel)) {
        rawsel=c();
        len=length(raw[,1]);
        index=1:len;
        rawind=cbind(raw,index);
    } else {
        rawsel=raw[sel,];
        len=length(rawsel[,1]);
        index=1:len;
        rawind=cbind(rawsel,index);
    };
    rawsub = rawind[,append(c(setcol,trgcol),selcols)];
    rm(len,index,rawind);gc(); # free memory
    sets=unique(rawsub[,setcol]);
    setlist=list();
    for (ss in sets) {
        selt = (rawsub[,setcol]==ss);
        newtrg=paste0(trgcol,ss);
        rawset = rawsub[selt,];
        colnames(rawset)[colnames(rawset)==trgcol] <- newtrg;
        print (paste("Set",ss," found cnt:",length(rawset[,1])));
        setlist=append(setlist,list(rawset));
    };
    rm(newtrg,rawsub,selt,rawset);gc(); # free memory
    rawmerge = Reduce(function(x, y) merge(x, y, by=selcols), setlist)
    print (paste("Initial cnt:",length(rawmerge[,1])));
    data=list();
    for (ss in sets) {
        newtrg=paste0(trgcol,ss);
        selind = rawmerge[,newtrg];
        print (paste("Set",ss," using cnt:",length(selind)));
        if (missing(sel)) {
            data=append(data,list(raw[selind,]));
        } else {
            data=append(data,list(rawsel[selind,]));
        };
    }
    rm (newtrg,selind,setlist,rawmerge,rawsel);gc(); # free memory
    return (data);  # return a list of sets that have overlapping data
}

equals <- function(raw,areacol,trg,ll) {
  a <- raw[,areacol];
  if (missing(ll)) {
     output <- (! is.na(a) & a == trg);
  } else {
     output <- (! is.na(a) & a == trg & ll);
  }
  if (sum(output)==0) {
      print (paste("No ",trg," stations found. Stations:"));
      print(unique(raw[,areacol]));
  }
  return (output);
}

below <- function(raw,altcol,thres,ll) {
  g <- as.numeric(raw[,altcol]);
  t <- as.numeric(thres);
  if (missing(ll)) {
     output <- (! is.na(g) & g  <= t);
  } else {
     output <- (! is.na(g) & g <= t & ll);
  }
  return (output);
}

above <- function(raw,altcol,thres,ll) {
  g <- as.numeric(raw[,altcol]);
  t <- as.numeric(thres);
  if (missing(ll)) {
     output <- (! is.na(g) & g > t);
  } else {
     output <- (! is.na(g) & g > t & ll);
  }
  return (output);
}

# thinned data
thinned <- function (len,ll) { 
  set.seed(1);
  thin_sample=(1:len);
  if (missing(ll)) {
    thin_raw <- thin_sample;
  } else {
    thin_raw <- thin_sample[ll];
  };
  thin_n = min(c(500,length(thin_raw)));
  print (paste("Thinning:",nrow(raw),"->",length(thin_raw)," -> ",thin_n));
  thin_extract <- sample(thin_raw,thin_n, replace=FALSE)
  return (thin_extract);
}
#

getDTG <- function (epoch) { # expects seconds - not milliseconds...
#   return(as.POSIXct(epoch, origin="1970-01-01"));#"2012-11-04 22:32:00 CST"
   return(as.Date(as.POSIXct(epoch, origin="1970-01-01"))); # "2012-11-05" 
}

getMinMaxDTGs <- function (raw) {
    mi <- min(raw);
    ma <- max(raw);
    mid= toString(getDTG(mi));
    mad= toString(getDTG(ma));
    print (paste("Found dates from:",mid," to ",mad));
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
    };
    rm(req,cc,ss,ot,ll,ts,os,ns,ff);gc(); # free memory
    #print(paste("Xvalue=",xx," Dtg=",dd));
    return (list(xx,dd));
}

fadeColor <- function(n,i) { # fade to "white"
    if (missing(i)) {
        return (colorRampPalette(c(palette()[[n]],"white"))(3)[[2]]);
    } else {
        return (colorRampPalette(c(palette()[[n]],"white"))(i+1)[[i]]);
    }
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

getValid <- function (data,obscol,modcol,min,max) {
    lend=length(data);
    lenv=0;
    if (lend > 0) {lenv=length(data[[1]][,1]) };
    valid <- rep(TRUE,lenv);
    print (paste("Looping sets:",length(data)));
    for (dd in 1:length(data)) {
        obs <- data[[dd]][,obscol];
        mod <- data[[dd]][,modcol];
        valid <- (valid & !is.na(obs) & !is.na(mod) & obs>min & obs<max & mod>min & mod<max);
        print (paste("Valid:",sum(valid)));
    }
    rm(lend,lenv,obs,mod);gc(); # free memory
    return (valid);
}    
getValidPred <- function (data,obscol,modcol,predcol,min,max) {
    lend=length(data);
    lenv=0;
    if (lend > 0) {lenv=length(data[[1]][,1]) };
    valid <- rep(TRUE,lenv);
    for (dd in 1:length(data)) {
        obs <- data[[dd]][,obscol];
        mod <- data[[dd]][,modcol];
        pred <- as.numeric(data[[dd]][,predcol]);
        valid <- (valid & !is.na(obs) & !is.na(mod) & ! is.na(pred)
            & obs>min & obs<max & mod>min & mod<max);
    }
    rm(lend,lenv,obs,mod,pred);gc(); # free memory
    return (valid);
}    

###################### score plot

predictorPlot <- function(filename,attr,leg,data,title,setcol,refcol,timecol,
                          predlab,predcol,preddir,obscol,modcol,lab,min,max,...) {
    npred <- 10; # number of height bins
    lst=list(...); # "label1",selection1,"label2",selection2...
    inp <- matrix(lst,ncol=2,byrow=TRUE);
    ilab <- inp[,1]; # labels
    isel <- inp[,2]; # selection
    if (length(ilab)==1) {
        title=paste0("'",title,"'\n",ilab[[1]]);
        ilab[[1]]="";
    } else {
        title=paste0("'",title,"'");
    };
    valid <- getValidPred(data,obscol,modcol,predcol,min,max);
    print (paste("Number of stations (profile):          ",sum(valid)));
    if (sum(valid)>1) {
        if (length(isel) == 0 ) {isel <- list(valid);ilab <- list("");};
        dtgs <- getMinMaxDTGs(data[[1]][valid,timecol]);
        ;# make data statistics...
        pred   <- as.numeric(data[[1]][,predcol]);
        bpred  <- quantile(pred,probs = seq(0, 1, by = 1.0/npred),na.rm=TRUE)
        qpred  <- findInterval(pred,bpred);
        fact   <- c();
        for (pp in 1:npred) {
            fact[[pp]] = 1/(bpred[[pp+1]]-bpred[[pp]]);
        }
        #print (paste("Factor:",fact));
        ;# initialise data arrays...
        yl    <- list();
        xmel  <- list();
        xsdel <- list();
        xrmsl <- list();
        xmael <- list();
        xcntl <- list();
        ycntl <- list();
        setl  <- list();
        il    <- list();
        xr    <- c();
        yr    <- c();
        statl <- list();
        for (ii in 1:length(isel)) {  # loop over selections
            for (dd in 1:length(data)) {  # loop over datasets
                set=data[[dd]][1,setcol];
                obs <- data[[dd]][,obscol];
                mod <- data[[dd]][,modcol];
                y    <- c();
                xme  <- c();
                xsde <- c();
                xrms <- c();
                xmae <- c();
                xcnt <- c();
                ycnt <- c();
                ;# overall statistics
                dsel <-  (isel[[ii]] & valid );
                d    <- (mod[dsel]-obs[dsel]);
                dme  <- round(mean(d),2);
                dsde <- round(sd(d),2) ;
                drms <- round(sqrt(mean(d^2)),2);
                dmae <- round(mean(abs(d)),2);
                dcnt <- pretty(sum(dsel));
                stat <- paste0("me=",dme,",sde=",dsde,",mae=",dmae,",cnt=",dcnt);
                #print(paste("Sel:",ii," stat:",stat));
                #print (pred);
                #print (qpred);
                #print (bpred);
                first=TRUE;
                lastx=0;
                lasty=0;
                for (ll in 1:npred) {    # loop over heights
                    dsel <- (isel[[ii]] & valid & !is.na(qpred) & qpred == ll );
                    #print (paste("Level:",ll));
                    #print (pred[dsel]);
                    if (sum(dsel)>2) {
                        if (first) {
                            xcnt <- append(xcnt,0);
                            ycnt <- append(ycnt,bpred[[ll]]);
                            first=FALSE;
                        }
                        y    <- append(y,mean(pred[dsel]));
                        d    <- (mod[dsel]-obs[dsel]);
                        xme  <- append(xme,  mean(d) );
                        xsde <- append(xsde, sd(d) );
                        xrms <- append(xrms, sqrt(mean(d^2)) );
                        xmae <- append(xmae, mean(abs(d)) );
                        xcnt <- append(xcnt,length(d)*fact[[ll]]);
                        xcnt <- append(xcnt,length(d)*fact[[ll]]);
                        ycnt <- append(ycnt,bpred[[ll]]);
                        ycnt <- append(ycnt,bpred[[ll+1]]);
                        lasty=bpred[[ll+1]]
                    };
                };
                if (! first) {
                    xcnt <- append(xcnt,lastx);
                    ycnt <- append(ycnt,lasty);
                };
                if (length(y)>0) {
                    ;### store all values... and handle range 
                    xm=max(xcnt);
                    xcnt=xcnt/xm;
                    yl    <- append(yl,list(y));
                    xmel  <- append(xmel,list(xme));
                    xsdel <- append(xsdel,list(xsde));
                    xrmsl <- append(xrmsl,list(xrms));
                    xmael <- append(xmael,list(xmae));
                    xcntl <- append(xcntl,list(xcnt));
                    ycntl <- append(ycntl,list(ycnt));
                    setl  <- append(setl,list(set));
                    il    <- append(il,list(ii));
                    yr    <- range( append(yr,y));
                    #yr    <- range( append(ycnt,y));
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
            if (preddir == "descending") {
                plot(yr,xr,type="n",ylab=paste(lab,"SDE and BIAS, model-observation"),xlab=predlab,xlim=rev(yr),las=1);
            } else {
                plot(yr,xr,type="n",ylab=paste(lab,"SDE and BIAS, model-observation"),xlab=predlab,xlim=yr,las=1);
            };
            title(title);
            legs=c();
            cols=c();
            ltys=c();
            lwds=c();
            stats= c();
            #print(paste("Selections:",length(setl)));
            for (jj in 1:length(setl)) {
                y    <- yl[[jj]];
                xme  <- xmel[[jj]];
                xsde <- xsdel[[jj]];
                xrms <- xrmsl[[jj]];
                xmae <- xmael[[jj]];
                xcnt <- xcntl[[jj]];
                ycnt <- ycntl[[jj]];
                ss   <- setl[[jj]];
                ii   <- il[[jj]];
                l    <- ilab[[ii]];
                stat <- statl[[jj]];
                xcnt=(xcnt*0.1 - 0.035)*(xr[[2]]-xr[[1]]) + xr[[1]];
###x <- as.POSIXct(x, origin="1970-01-01");
                #print(paste("Plotting:",jj,"X:",xsde,"Y:",y));
                lines(y,xsde,type="l",lty=ss,col=ii,lwd=2);        # stde
                lines(y,xme,type="l",lty=ss,col=fadeColor(ii),lwd=2); # bias
                # constrain the count-range
                minV=yr[[1]];
                maxV=yr[[2]];
                zcnt <- sapply(ycnt, function(y) min(max(y,minV),maxV))
                # add count bar
                polygon(zcnt,xcnt,type="l",col=fadeColor(1,3),border=NA); # cnt
                if (l=="") {
                    legs=append(legs,paste0(leg[ss]));
                } else {
                    legs=append(legs,paste0(leg[ss]," (",l,")"));
                }; 
                cols=append(cols,ii);
                ltys=append(ltys,ss);
                lwds=append(lwds,2); # 3/0.75
                stats=append(stats,stat);
            };
            lines(yr,c(0.,0.),type="l",lty=2,col=gray(0.75));
            info=paste("From",toString(dtgs[1]),
                       "to",as.Date(dtgs[2],origin="1970-01-01"),paste0("(",dtgs[3],")"));
            legend("bottomright",legend=info,bty="n",cex=0.75*par("cex"));
            if (length(legs)>0) {
                print ("Adding legend.");
                legend("topleft",legend=legs,col=cols,lty=ltys,lwd=lwds,
                       bty="n",cex=0.75*par("cex"),seg.len=2.5);
                legend("topright",legend=stats,bty="n",cex=0.75*par("cex"));
            };
            closePlot();
        } else {
            print (paste("$$$ No plot data for ",filename));
        }
    } else {
        print (paste("$$$ No valid data for ",filename));
    }
    rm(npred,lst,inp,ilab,isel,title,valid,dtgs,pred,bpred,qpred);
    rm(yl,xmel,xsdel,xrmsl,xmael,xcntl,ycntl,setl,il,xr,yr,statl);
    rm(set,obs,mod,legs,cols,ltys,lwds,stats);
    rm(xme,xsde,xrms,xmae,xcnt,ycnt);
    rm(dsel,dme,dsde,drms,dmae,dcnt,y,d);
    rm(ss,l,stat,info);gc();
};

###################### scatter plot (... -> ["label",selection]...)

scatterPlot <- function(filename,attr,leg,data,title,setcol,refcol,timecol,
                        altcol,obscol,modcol,obslab,modlab,min,max,...) {
    lst=list(...);
    inp <- matrix(lst,ncol=2,byrow=TRUE);
    ilab <- inp[,1]; # labels
    isel <- inp[,2]; # selection
    if (length(ilab)==1) {
        title=paste0("'",title,"'\n",ilab[[1]]);
        ilab[[1]]="";
    } else {
        title=paste0("'",title,"'");
    };
    # loop over sets
    valid <- getValid(data,obscol,modcol,min,max);
    print (paste("Number of stations (scatter):          ",sum(valid)));
    legs=c();
    cols=c();
    pchs=c();
    stats=c();
    if (length(isel) == 0 ) {isel <- list(valid);ilab <- list("");};
    if (sum(valid)>1) {
        dtgs <- getMinMaxDTGs(data[[1]][valid,timecol]);
        xl <- list();
        yl <- list();
        xr <- c();
        yr <- c();
        rr <- c();
        tcnt <- 0;
        for (ii in 1:length(isel)) { # loop over selections
            dsel <- (isel[[ii]] & valid);
            td <- thinned(nrow(data[[1]]),dsel);
            for (dd in 1:length(data)) {  # loop over datasets
                set=data[[dd]][1,setcol];
                obs <- data[[dd]][,obscol];
                mod <- data[[dd]][,modcol];
                xr <- range(append(xr,obs[valid]));
                yr <- range(append(yr,mod[valid]));
                rr=range(append(rr,union(xr,yr)));
                l <- ilab[[ii]];
                d    <- (mod[dsel]-obs[dsel]);
                dme  <- round(mean(d),2);
                dsde <- round(sd(d),2) ;
                drms <- round(sqrt(mean(d^2)),2);
                dmae <- round(mean(abs(d)),2);
                dcnt <- pretty(sum(dsel));
                stat <- paste0("me=",dme,",sde=",dsde,",mae=",dmae,",cnt=",dcnt);
                print (paste0("Set=",set," count=",pretty(sum(dsel))," (",l,")"));
                tcnt=tcnt+sum(dsel);
                if (sum(dsel) > 0) {
                    xl <- append(xl,list(obs[td]));
                    yl <- append(yl,list(mod[td]));
                    if (l=="") {
                        legs=append(legs,paste0(leg[set]));
                    } else {
                        legs=append(legs,paste0(leg[set]," (",l,")"));
                    };
                    cols=append(cols,ii);
                    pchs=append(pchs,set);
                    stats=append(stats,stat);
                };
            };
        };
        if (tcnt>0) {
            print ( paste("Tcnt:",tcnt));
            openPlot(filename,attr);
            plot(xr,yr,type="n",xlab=obslab,ylab=modlab,las=1);#,tck = 0.0
            title(title);
            lines(rr,rr,type="l",lty=2,col=gray(0.75));
            for (jj in 1:length(xl)) {
                x    <- xl[[jj]];
                y    <- yl[[jj]];
                pch  <- pchs[[jj]];
                col  <- cols[[jj]];
                lines(x,y,type="p",pch=pch,col=col);
            }
            info=paste("From",toString(dtgs[1]),
                       "to",as.Date(dtgs[2],origin="1970-01-01"),paste0("(",dtgs[3],")"));
            legend("bottomright",legend=info,bty="n",cex=0.75*par("cex"));
            if (length(legs)>0) {
                print ("Adding legend.");
                legend("topleft",legend=legs,col=cols,pch=pchs,
                       bty="n",cex=0.75*par("cex"),seg.len=2.5);
                legend("topright",legend=stats,bty="n",cex=0.75*par("cex"));
            };
            closePlot();
        };
    } else {
        print (paste("$$$ No data for ",filename));
    }
    rm(lst,inp,ilab,isel,title,valid,legs,cols,pcshs,stats);
    rm(set,obs,mod,xr,yr,rr,l,d,dme,dsde,drms,dmae,dcnt,stat);
    rm(xl,yl,xr,yr,rr,dsel,td,dd);gc(); # free memory

};

###################### score plot

scorePlot <- function(filename,attr,leg,data,title,setcol,refcol,timecol,
                      altcol,obscol,modcol,lab,min,max,...) {
    lst=list(...); # "label1",selection1,"label2",selection2...
    inp <- matrix(lst,ncol=2,byrow=TRUE);
    ilab <- inp[,1]; # labels
    isel <- inp[,2]; # selection
    if (length(ilab)==1) {
        title=paste0("'",title,"'\n",ilab[[1]]);
        ilab[[1]]="";
    } else {
        title=paste0("'",title,"'");
    };
    valid <- getValid(data,obscol,modcol,min,max);
    print (paste("Number of stations (score):          ",sum(valid)));
    if (sum(valid)>1) {
        if (length(isel) == 0 ) {isel <- list(valid);ilab <- list("");};
        dtgs <- getMinMaxDTGs(data[[1]][valid,timecol]);
        ;# make data statistics...
        lead  <- round((data[[1]][,timecol]-data[[1]][,refcol])/3600.0);
        leadu <- unique(lead[valid]);
        xl    <- list();
        ymel  <- list();
        yobsl <- list();
        ymodl <- list();
        ysdel <- list();
        yrmsl <- list();
        ymael <- list();
        ycntl <- list();
        setl    <- list();
        il    <- list();
        xr    <- c();
        yr    <- c();
        ys    <- c();
        statl <- list();
        print (paste("Looping:          ",length(isel)));
        for (ii in 1:length(isel)) {  # loop over selections
            for (dd in 1:length(data)) {  # loop over datasets
                set=data[[dd]][1,setcol];
                obs <- data[[dd]][,obscol];
                mod <- data[[dd]][,modcol];
                ret <- processLead(mod,obs,ilab,isel,ii,valid,set,leadu,lead,
                                   xl,ymel,yobsl,ymodl,ysdel,yrmsl,ymael,ycntl,
                                   setl,il,xr,yr,ys,statl);
                xl=ret$xl;
                ymel=ret$ymel;
                yobsl=ret$yobsl;
                ymodl=ret$ymodl;
                ysdel=ret$ysdel;
                yrmsl=ret$yrmsl;
                ymael=ret$ymael;
                ycntl=ret$ycntl;
                setl=ret$setl;
                il=ret$il;
                xr=ret$xr;
                yr=ret$yr;
                ys=ret$ys;
                statl=ret$statl;
            };
        };
        ;# make plot
        if (length(xr)>0) { # we have data
            openPlot(filename,attr);
            plot(xr,yr,type="n",xlab=paste("Lead time (h)"),ylab=paste(lab,"SDE and BIAS, model-observation"),las=1);#,tck = 0.0
            title(title);
            legs=c();
            cols=c();
            ltys=c();
            lwds=c();
            stats=c();
            for (jj in 1:length(setl)) {
                x    <- xl[[jj]];
                yme  <- ymel[[jj]];
                ysde <- ysdel[[jj]];
                yrms <- yrmsl[[jj]];
                ymae <- ymael[[jj]];
                ycnt <- ycntl[[jj]];
                ss   <- setl[[jj]];
                ii   <- il[[jj]];
                l    <- ilab[[ii]];
                stat <- statl[[jj]];
                lines(x,ysde,type="l",lty=ss,col=ii,lwd=2);        # stde
                lines(x,yme,type="l",lty=ss,col=fadeColor(ii),lwd=2); # bias
                if (l=="") {
                    legs=append(legs,paste0(leg[ss]));
                } else {
                    legs=append(legs,paste0(leg[ss]," (",l,")"));
                }; 
                cols=append(cols,ii);
                ltys=append(ltys,ss);
                lwds=append(lwds,2); # 3/0.75
                stats=append(stats,stat);
            };
            lines(xr,c(0.,0.),type="l",lty=2,col=gray(0.75));
            info=paste("From",toString(dtgs[1]),
                       "to",as.Date(dtgs[2],origin="1970-01-01"),paste0("(",dtgs[3],")"));
            legend("bottomright",legend=info,bty="n",cex=0.75*par("cex"));
            if (length(legs)>0) {
                print ("Adding legend.");
                legend("topleft",legend=legs,col=cols,lty=ltys,lwd=lwds,
                       bty="n",cex=0.75*par("cex"),seg.len=2.5);
                legend("topright",legend=stats,bty="n",cex=0.75*par("cex"));
            };
            closePlot();
        } else {
            print (paste("$$$ No plot data for ",filename));
        }
    } else {
        print (paste("$$$ No valid data for ",filename));
    }
    rm(lst,inp,ilab,isel,title,valid,lead,leadu);
    rm (xl,ymel,yobsl,ymodl,ysdel,yrmsl,ymael,ycntl,setl,il,xr,yr,ys,statl);
    rm(set,obs,mod,ret,legs,cols,ltys,lwds,stats);
    rm(x,yme,ysde,yrms,ymae,ycnt,ss,ii,l,stat,cols,ltys,lwds,info);gc();
};

###################### series plot
    
seriesPlot <- function(filename,attr,leg,data,title,setcol,refcol,timecol,
                       altcol,obscol,modcol,lab,min,max,...) {
    lst=list(...); # "label1",selection1,"label2",selection2...
    inp <- matrix(lst,ncol=2,byrow=TRUE);
    ilab <- inp[,1]; # labels
    isel <- inp[,2]; # selection
    if (length(ilab)==1) {
        title=paste0("'",title,"'\n",ilab[[1]]);
        ilab[[1]]="";
    } else {
        title=paste0("'",title,"'");
    };
    valid <- getValid(data,obscol,modcol,min,max);
    print (paste("Number of stations (series):          ",sum(valid)));
    if (sum(valid)>1) {
        if (length(isel) == 0 ) {isel <- list(valid);ilab <- list("");};
        dtgs <- getMinMaxDTGs(data[[1]][valid,timecol]);
        ;# make data statistics...
        dtg   <- floor((data[[1]][,timecol]/86400.0));
        dtgu  <- unique(dtg[valid]);
        xdtg  <- getAxisDates(dtgu);
        xl    <- list();
        ymel  <- list();
        yobsl <- list();
        ymodl <- list();
        ysdel <- list();
        yrmsl <- list();
        ymael <- list();
        ycntl <- list();
        setl    <- list();
        il    <- list();
        xr    <- c();
        yr    <- c();
        ys    <- c();
        statl <- list();
        for (ii in 1:length(isel)) {  # loop over selections
            for (dd in 1:length(data)) {  # loop over datasets
                set=data[[dd]][1,setcol];
                obs <- data[[dd]][,obscol];
                mod <- data[[dd]][,modcol];
                ret <- processTime(mod,obs,ilab,isel,ii,valid,set,dtgu,dtg,
                                   xl,ymel,yobsl,ymodl,ysdel,yrmsl,ymael,ycntl,
                                   setl,il,xr,yr,ys,statl);
                xl=ret$xl;
                ymel=ret$ymel;
                yobsl=ret$yobsl;
                ymodl=ret$ymodl;
                ysdel=ret$ysdel;
                yrmsl=ret$yrmsl;
                ymael=ret$ymael;
                ycntl=ret$ycntl;
                setl=ret$setl;
                il=ret$il;
                xr=ret$xr;
                yr=ret$yr;
                ys=ret$ys;
                statl=ret$statl;
            };
        };
        ;# make plot
        if (length(xr)>0) { # we have data
            ###xr <- as.POSIXct(xr, origin="1970-01-01");
            print(paste("Plotting x=",xr," y=",ys));
            print (xr);
            print (ys);
            openPlot(filename,attr);
            plot(xr,ys,type="n",ylab=paste(lab,"SDE and BIAS, model-observation"),xlab="time",xaxt="n",las=1);
            axis(1,at=xdtg[[1]],labels=xdtg[[2]], cex.axis = .75*par("cex"));
            title(title);
            legs=c();
            cols=c();
            ltys=c();
            lwds=c();
            stats=c();
            for (jj in 1:length(setl)) {
                x    <- xl[[jj]];
                yme  <- ymel[[jj]];
                ysde <- ysdel[[jj]];
                yrms <- yrmsl[[jj]];
                ymae <- ymael[[jj]];
                ycnt <- ycntl[[jj]];
                ss   <- setl[[jj]];
                ii   <- il[[jj]];
                l    <- ilab[[ii]];
                stat <- statl[[jj]];
                ###x <- as.POSIXct(x, origin="1970-01-01");
                lines(spline(x,ysde),type="l",lty=ss,col=ii,lwd=2);        # stde
                lines(spline(x,yme),type="l",lty=ss,col=fadeColor(ii),lwd=2); # bias
                if (l=="") {
                    legs=append(legs,paste0(leg[ss]));
                } else {
                    legs=append(legs,paste0(leg[ss]," (",l,")"));
                }; 
                cols=append(cols,ii);
                ltys=append(ltys,ss);
                lwds=append(lwds,2); # 3/0.75
                stats=append(stats,stat);
            };
            lines(xr,c(0.,0.),type="l",lty=2,col=gray(0.75));
            info=paste("From",toString(dtgs[1]),
                       "to",as.Date(dtgs[2],origin="1970-01-01"),paste0("(",dtgs[3],")"));
            legend("bottomright",legend=info,bty="n",cex=0.75*par("cex"));
            if (length(legs)>0) {
                print ("Adding legend.");
                legend("topleft",legend=legs,col=cols,lty=ltys,lwd=lwds,
                       bty="n",cex=0.75*par("cex"),seg.len=2.5);
                legend("topright",legend=stats,bty="n",cex=0.75*par("cex"));
            };
            closePlot();
        } else {
            print (paste("$$$ No plot data for ",filename));
        }
    } else {
        print (paste("$$$ No valid data for ",filename));
    }
    rm(lst,inp,ilab,isel,title,valid,dtgs,dtg,dtgu,xdtg);
    rm (xl,ymel,yobsl,ymodl,ysdel,yrmsl,ymael,ycntl,setl,il,xr,yr,ys,statl);
    rm(set,obs,mod,ret,legs,cols,ltys,lwds,stats);
    rm(x,yme,ysde,yrms,ymae,ycnt,ss,ii,l,stat,cols,ltys,lwds,info);gc();
};

###################### series plot
    
timePlot <- function(filename,attr,leg,data,title,setcol,refcol,timecol,
                     altcol,obscol,modcol,lab,min,max,...) {
    lst=list(...); # "label1",selection1,"label2",selection2...
    inp <- matrix(lst,ncol=2,byrow=TRUE);
    ilab <- inp[,1]; # labels
    isel <- inp[,2]; # selection
    if (length(ilab)==1) {
        title=paste0("'",title,"'\n",ilab[[1]]);
        ilab[[1]]="";
    } else {
        title=paste0("'",title,"'");
    };
    valid <- getValid(data,obscol,modcol,min,max);
    print (paste("Number of stations (time):          ",sum(valid)));
    if (sum(valid)>1) {
        if (length(isel) == 0 ) {isel <- list(valid);ilab <- list("");};
        dtgs <- getMinMaxDTGs(data[[1]][valid,timecol]);
        ;# make data statistics...
        dtg   <- floor((data[[1]][,timecol]/86400.0));
        dtgu  <- unique(dtg[valid]);
        xdtg  <- getAxisDates(dtgu);
        xl    <- list();
        ymel  <- list();
        yobsl <- list();
        ymodl <- list();
        ysdel <- list();
        yrmsl <- list();
        ymael <- list();
        ycntl <- list();
        setl    <- list();
        il    <- list();
        xr    <- c();
        yr    <- c();
        ys    <- c();
        statl <- list();
        for (ii in 1:length(isel)) {  # loop over selections
            for (dd in 1:length(data)) {  # loop over datasets
                set=data[[dd]][1,setcol];
                obs <- data[[dd]][,obscol];
                mod <- data[[dd]][,modcol];
                ret <- processTime(mod,obs,ilab,isel,ii,valid,set,dtgu,dtg,
                                   xl,ymel,yobsl,ymodl,ysdel,yrmsl,ymael,ycntl,
                                   setl,il,xr,yr,ys,statl);
                xl=ret$xl;
                ymel=ret$ymel;
                yobsl=ret$yobsl;
                ymodl=ret$ymodl;
                ysdel=ret$ysdel;
                yrmsl=ret$yrmsl;
                ymael=ret$ymael;
                ycntl=ret$ycntl;
                setl=ret$setl;
                il=ret$il;
                xr=ret$xr;
                yr=ret$yr;
                ys=ret$ys;
                statl=ret$statl;
            };
        };
        ;# make plot
        if (length(xr)>0) { # we have data
            ###xr <- as.POSIXct(xr, origin="1970-01-01");
            openPlot(filename,attr);
            plot(xr,yr,type="n",ylab=paste(lab,"MEAN,"),xlab="time",xaxt="n",las=1);
            axis(1,at=xdtg[[1]],labels=xdtg[[2]], cex.axis = .75*par("cex"));
            title(title);
            legs=c();
            cols=c();
            ltys=c();
            lwds=c();
            stats=c();
            for (jj in 1:length(setl)) {
                x    <- xl[[jj]];
                yme  <- ymel[[jj]];
                yobs <- yobsl[[jj]];
                ymod <- ymodl[[jj]];
                ysde <- ysdel[[jj]];
                yrms <- yrmsl[[jj]];
                ymae <- ymael[[jj]];
                ycnt <- ycntl[[jj]];
                ss   <- setl[[jj]];
                ii   <- il[[jj]];
                l    <- ilab[[ii]];
                stat <- statl[[jj]];
                ###x <- as.POSIXct(x, origin="1970-01-01");
                lines(spline(x,ymod),type="l",lty=ss,col=ii,lwd=2);            # model
                lines(spline(x,yobs),type="l",lty=ss,col=fadeColor(ii),lwd=3); # observations
                # model
                if (l=="") {
                    legs=append(legs,paste0(leg[ss]));
                } else {
                    legs=append(legs,paste0(leg[ss]," (",l,")"));
                }; 
                cols=append(cols,ii);
                ltys=append(ltys,ss);
                lwds=append(lwds,2); # 3/0.75
                stats=append(stats,stat);
                # obs
                legs=append(legs,paste0("obs"));
                cols=append(cols,fadeColor(ii));
                ltys=append(ltys,ss);
                lwds=append(lwds,3); # 3/0.75
                stats=append(stats,"");
            };
            lines(xr,c(0.,0.),type="l",lty=2,col=gray(0.75));
            info=paste("From",toString(dtgs[1]),
                       "to",as.Date(dtgs[2],origin="1970-01-01"),paste0("(",dtgs[3],")"));
            legend("bottomright",legend=info,bty="n",cex=0.75*par("cex"));
            if (length(legs)>0) {
                print ("Adding legend.");
                legend("topleft",legend=legs,col=cols,lty=ltys,lwd=lwds,
                       bty="n",cex=0.75*par("cex"),seg.len=2.5);
                legend("topright",legend=stats,bty="n",cex=0.75*par("cex"));
            };
            closePlot();
        } else {
            print (paste("$$$ No plot data for ",filename));
        }
    } else {
        print (paste("$$$ No valid data for ",filename));
    }
    rm(lst,inp,ilab,isel,title,valid,dtgs,dtg,dtgu,xdtg);
    rm (xl,ymel,yobsl,ymodl,ysdel,yrmsl,ymael,ycntl,setl,il,xr,yr,ys,statl);
    rm(set,obs,mod,ret,legs,cols,ltys,lwds,stats);
    rm(x,yme,ysde,yrms,ymae,ycnt,ss,ii,l,stat,cols,ltys,lwds,info);gc();
};

###################### historgram plot

histPlot <- function(filename,attr,leg,data,title,setcol,refcol,timecol,
                        altcol,obscol,modcol,lab,min,max,...) {
    lst=list(...);
    inp <- matrix(lst,ncol=2,byrow=TRUE);
    ilab <- inp[,1]; # labels
    isel <- inp[,2]; # selection
    if (length(ilab)==1) {
        title=paste0("'",title,"'\n",ilab[[1]]);
        ilab[[1]]="";
    } else {
        title=paste0("'",title,"'");
    };
    valid <- getValid(data,obscol,modcol,min,max);
    print (paste("Number of stations (hist):          ",sum(valid)));
    xl  <- list();
    yl  <- list();
    setl  <- list();
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
        dtgs <- getMinMaxDTGs(data[[1]][valid,timecol]);
        #title();
        for (ii in 1:length(isel)) { # loop over selections
            for (dd in 1:length(data)) {  # loop over datasets
                set=data[[dd]][1,setcol];
                obs <- data[[dd]][,obscol];
                mod <- data[[dd]][,modcol];
                ret <- processHist(mod,obs,ilab,isel,ii,valid,set,
                                   xl,yl,setl,il,xr,yr,legs,cols,
                                   ltys,lwds,stats);
                xl    = ret$xl;
                yl    = ret$yl;
                setl    = ret$setl;
                il    = ret$il;
                xr    = ret$xr;
                yr    = ret$yr;
                legs  = ret$legs;
                cols  = ret$cols;
                ltys  = ret$ltys;
                lwds  = ret$lwds;
                stats = ret$stat;
            };
        };
        if (length(xr)>0) { # we have data
            openPlot(filename,attr);
            plot(xr,yr,type="n",
                 main=title,
                 xlab=paste(label,"model-observation"),
                 ylab="Density",las=1);
            for (jj in 1:length(xl)) {
                x    <- xl[[jj]];
                y    <- yl[[jj]];
                ss   <- setl[[jj]];
                ii   <- il[[jj]];
                lines(x,y,type="l",lty=ss,col=ii,lwd=2);
            };
            lines(c(0.,0.),yr,type="l",lty=2,col=gray(0.75));
            info=paste("From",toString(dtgs[1]),
                       "to",as.Date(dtgs[2],origin="1970-01-01"),paste0("(",dtgs[3],")"));
            legend("bottomright",legend=info,bty="n",cex=0.75*par("cex"));
            if (length(legs)>0) {
                print ("Adding legend.");
                legend("topleft",legend=legs,col=cols,lty=ltys,
                       lwd=lwds,bty="n",cex=0.75*par("cex"),seg.len=2.5);
                legend("topright",legend=stats,bty="n",cex=0.75*par("cex"));
            };
            closePlot();
        };
    } else {
        print (paste("$$$ No data for ",filename));
    }
    rm(lst,inp,ilab,isel,title,valid);
    rm (xl,yl,setl,il,xr,yr,statl);
    rm(set,obs,mod,ret,legs,cols,ltys,lwds,stats);
    rm(info);gc();
};

###################### vertical profile plot

profilePlot <- function(filename,attr,leg,data,title,setcol,refcol,timecol,
                       hgtlab,hgtcol,hgtdir,obscol,modcol,lab,min,max,...) {
    nhgt <- 10; # number of height bins
    lst=list(...); # "label1",selection1,"label2",selection2...
    inp <- matrix(lst,ncol=2,byrow=TRUE);
    ilab <- inp[,1]; # labels
    isel <- inp[,2]; # selection
    if (length(ilab)==1) {
        title=paste0("'",title,"'\n",ilab[[1]]);
        ilab[[1]]="";
    } else {
        title=paste0("'",title,"'");
    };
    valid <- getValidPred(data,obscol,modcol,hgtcol,min,max);
    print (paste("Number of stations (profile):          ",sum(valid)));
    if (sum(valid)>1) {
        if (length(isel) == 0 ) {isel <- list(valid);ilab <- list("");};
        dtgs <- getMinMaxDTGs(data[[1]][valid,timecol]);
        ;# make data statistics...
        hgt   <- as.numeric(data[[1]][,hgtcol]);
        bhgt  <- quantile(hgt,probs = seq(0, 1, by = 1.0/nhgt),na.rm=TRUE)
        qhgt  <- findInterval(hgt,bhgt);
        ;# initialise data arrays...
        yl    <- list();
        xmel  <- list();
        xsdel <- list();
        xrmsl <- list();
        xmael <- list();
        xcntl <- list();
        setl    <- list();
        il    <- list();
        xr    <- c();
        yr    <- c();
        statl <- list();
        for (ii in 1:length(isel)) {  # loop over selections
            for (dd in 1:length(data)) {  # loop over datasets
                set=data[[dd]][1,setcol];
                obs <- data[[dd]][,obscol];
                mod <- data[[dd]][,modcol];
                y    <- c();
                xme  <- c();
                xsde <- c();
                xrms <- c();
                xmae <- c();
                xcnt <- 0;
                ;# overall statistics
                dsel <-  (isel[[ii]] & valid );
                d    <- (mod[dsel]-obs[dsel]);
                dme  <- round(mean(d),2);
                dsde <- round(sd(d),2) ;
                drms <- round(sqrt(mean(d^2)),2);
                dmae <- round(mean(abs(d)),2);
                dcnt <- pretty(sum(dsel));
                stat <- paste0("me=",dme,",sde=",dsde,",mae=",dmae,",cnt=",dcnt);
                #print(paste("Sel:",ii," stat:",stat));
                #print (hgt);
                #print (qhgt);
                #print (bhgt);
                for (ll in 1:nhgt) {    # loop over heights
                    dsel <- (isel[[ii]] & valid & !is.na(qhgt) & qhgt == ll );
                    #print (paste("Level:",ll));
                    #print (hgt[dsel]);
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
                    setl  <- append(setl,list(set));
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
                plot(xr,yr,type="n",xlab=paste(lab,"SDE and BIAS, model-observation"),ylab=hgtlab,ylim=rev(yr),las=1);
            } else {
                plot(xr,yr,type="n",xlab=paste(lab,"SDE and BIAS, model-observation"),ylab=hgtlab,ylim=yr,las=1);
            };
            title(title);
            legs=c();
            cols=c();
            ltys=c();
            lwds=c();
            stats= c();
            #print(paste("Selections:",length(setl)));
            for (jj in 1:length(setl)) {
                y    <- yl[[jj]];
                xme  <- xmel[[jj]];
                xsde <- xsdel[[jj]];
                xrms <- xrmsl[[jj]];
                xmae <- xmael[[jj]];
                xcnt <- xcntl[[jj]];
                ss   <- setl[[jj]];
                ii   <- il[[jj]];
                l    <- ilab[[ii]];
                stat <- statl[[jj]];
                ###x <- as.POSIXct(x, origin="1970-01-01");
                #print(paste("Plotting:",jj,"X:",xsde,"Y:",y));
                lines(xsde,y,type="l",lty=ss,col=ii,lwd=2);        # stde
                lines(xme,y,type="l",lty=ss,col=fadeColor(ii),lwd=2); # bias
                if (l=="") {
                    legs=append(legs,paste0(leg[ss]));
                } else {
                    legs=append(legs,paste0(leg[ss]," (",l,")"));
                }; 
                cols=append(cols,ii);
                ltys=append(ltys,ss);
                lwds=append(lwds,2); # 3/0.75
                stats=append(stats,stat);
            };
            lines(c(0.,0.),yr,type="l",lty=2,col=gray(0.75));
            info=paste("From",toString(dtgs[1]),
                       "to",as.Date(dtgs[2],origin="1970-01-01"),paste0("(",dtgs[3],")"));
            legend("bottomright",legend=info,bty="n",cex=0.75*par("cex"));
            if (length(legs)>0) {
                print ("Adding legend.");
                legend("topleft",legend=legs,col=cols,lty=ltys,lwd=lwds,
                       bty="n",cex=0.75*par("cex"),seg.len=2.5);
                legend("topright",legend=stats,bty="n",cex=0.75*par("cex"));
            };
            closePlot();
        } else {
            print (paste("$$$ No plot data for ",filename));
        }
    } else {
        print (paste("$$$ No valid data for ",filename));
    }
    rm(nhgt,lst,inp,ilab,isel,title,valid,dtgs,hgt,bhgt,qhgt);
    rm(yl,xmel,xsdel,xrmsl,xmael,xcntl,setl,il,xr,yr,statl);
    rm(set,obs,mod,ret,legs,cols,ltys,lwds,stats);
    rm(y,xme,xsde,xrms,xmae,xcnt);
    rm(dsel,d,dme,dsde,drms,dmae,dcnt,y,d);
    rm(ss,ii,l,stat,cols,ltys,lwds,info);gc();
};

########################## auxiliary plot function


processLead <- function (mod,obs,ilab,isel,ii,valid,set,leadu,lead,
                         xl,ymel,yobsl,ymodl,ysdel,yrmsl,ymael,ycntl,
                         setl,il,xr,yr,ys,statl) {
    print (paste("Processing lead"));
    x    <- c();
    yme  <- c();
    yobs <- c();
    ymod <- c();
    ysde <- c();
    yrms <- c();
    ymae <- c();
    ycnt <- 0;
    ;# overall statistics
    dsel <-  (isel[[ii]] & valid );
    d    <- (mod[dsel]-obs[dsel]);
    dme  <- round(mean(d),2);
    dsde <- round(sd(d),2) ;
    drms <- round(sqrt(mean(d^2)),2);
    dmae <- round(mean(abs(d)),2);
    dcnt <- pretty(sum(dsel));
    stat <- paste0("me=",dme,",sde=",dsde,",mae=",dmae,",cnt=",dcnt);
                                        #print (is.na(isel[[ii]]));
    for (ll in sort(leadu)) {    # loop over lead times
        dsel <- (isel[[ii]] & valid & lead == ll );
        if (sum(dsel)>2) {
            x    <- append(x,ll);
            d    <- (mod[dsel]-obs[dsel]);
            yme  <- append(yme,  mean(d) );
            ysde <- append(ysde, sd(d) );
            yobs <- append(yobs, mean(obs[dsel]) );
            ymod <- append(ymod, mean(mod[dsel]) );
            yrms <- append(yrms, sqrt(mean(d^2)) );
            ymae <- append(ymae, mean(abs(d)) );
            ycnt <- ycnt + length(d);
        };
    };
    if (length(x)>0) {
        ;### store all values... and handle range 
        xl    <- append(xl,list(x));
        ymel  <- append(ymel,list(yme));
        yobsl <- append(yobsl,list(yobs));
        ymodl <- append(ymodl,list(ymod));
        ysdel <- append(ysdel,list(ysde));
        yrmsl <- append(yrmsl,list(yrms));
        ymael <- append(ymael,list(ymae));
        ycntl <- append(ycntl,list(ycnt));
        setl    <- append(setl,list(set));
        il    <- append(il,list(ii));
        xr    <- range( append(xr,x));
        yr    <- range( append(yr,yme));
        yr    <- range( append(yr,ysde));
        statl <- append(statl,list(stat));
    };
    rm(x,yme,yobs,ymod,ysde,yrms,ymae,ycnt);
    rm(dsel,d,dme,dsde,drms,dmae,dcnt,stat);gc();
    return (list("xl"=xl,
                 "ymel"=ymel,
                 "yobsl"=yobsl,
                 "ymodl"=ymodl,
                 "ysdel"=ysdel,
                 "yrmsl"=yrmsl,
                 "ymael"=ymael,
                 "ycntl"=ycntl,
                 "setl"=setl,
                 "il"=il,
                 "xr"=xr,
                 "yr"=yr,
                 "statl"=statl));
};

processTime <- function (mod,obs,ilab,isel,ii,valid,set,dtgu,dtg,
                         xl,ymel,yobsl,ymodl,ysdel,yrmsl,ymael,ycntl,
                         setl,il,xr,yr,ys,statl) {
    x    <- c();
    yme  <- c();
    yobs <- c();
    ymod <- c();
    ysde <- c();
    ysde <- c();
    yrms <- c();
    ymae <- c();
    ycnt <- 0;
                                        # overall statistics
    dsel <-  (isel[[ii]] & valid );
    d    <- (mod[dsel]-obs[dsel]);
    dme  <- round(mean(d),2);
    dsde <- round(sd(d),2) ;
    drms <- round(sqrt(mean(d^2)),2);
    dmae <- round(mean(abs(d)),2);
    dcnt <- pretty(sum(dsel));
    stat <- paste0("me=",dme,",sde=",dsde,",mae=",dmae,",cnt=",dcnt);
    for (ll in sort(dtgu)) {    # loop over times
        dsel <- (isel[[ii]] & valid & dtg == ll );
        if (sum(dsel)>2) {
            x    <- append(x,ll);
            d    <- (mod[dsel]-obs[dsel]);
            yme  <- append(yme,  mean(d) );
            yobs <- append(yobs, mean(obs[dsel]) );
            ymod <- append(ymod, mean(mod[dsel]) );
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
        yobsl <- append(yobsl,list(yobs));
        ymodl <- append(ymodl,list(ymod));
        ysdel <- append(ysdel,list(ysde));
        yrmsl <- append(yrmsl,list(yrms));
        ymael <- append(ymael,list(ymae));
        ycntl <- append(ycntl,list(ycnt));
        setl    <- append(setl,list(set));
        il    <- append(il,list(ii));
        xr    <- range( append(xr,x));
        yr    <- range( append(yr,yobs));
        yr    <- range( append(yr,ymod));
        ys    <- range( append(ys,yme));
        ys    <- range( append(ys,ysde));
        statl <- append(statl,list(stat));
    };
    rm(x,yme,yobs,ymod,ysde,yrms,ymae,ycnt);
    rm(dsel,d,dme,dsde,drms,dmae,dcnt,stat);gc();
    return (list("xl"=xl,
                 "ymel"=ymel,
                 "yobsl"=yobsl,
                 "ymodl"=ymodl,
                 "ysdel"=ysdel,
                 "yrmsl"=yrmsl,
                 "ymael"=ymael,
                 "ycntl"=ycntl,
                 "setl"=setl,
                 "il"=il,
                 "xr"=xr,
                 "yr"=yr,
                 "ys"=ys,
                 "statl"=statl));  
};

processHist <- function (mod,obs,ilab,isel,ii,valid,set,
                         xl,yl,setl,il,xr,yr,legs,cols,
                         ltys,lwds,stats) {
    l <- ilab[[ii]];
    dsel <- (isel[[ii]] & valid);
    print (paste0("Set=",set," count=",pretty(sum(dsel))," (",l,")"));
    if (sum(dsel) > 0) {
        d <- mod[dsel]-obs[dsel];
        pp <- hist(d,nclass=50,plot=FALSE);
        lenp <- length(pp$mids);
        x <- c();
        y <- c();
        for (jj in 1:lenp) {
            x <- c(x,pp$breaks[jj],pp$breaks[jj+1]);
            y <- c(y,pp$density[jj],pp$density[jj]);
        };
        if (length(x)>0) {
            ;### store all values... and handle range 
            xl    <- append(xl,list(x));
            yl    <- append(yl,list(y));
            setl    <- append(setl,list(set));
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
                legs=append(legs,paste0(leg[set]));
            } else {
                legs=append(legs,paste0(leg[set]," (",l,")"));
            };
            cols = append(cols,ii);
            ltys = append(ltys,set);
            lwds = append(lwds,2); # 3/0.75
            stats= append(stats,stat);
        };
    };
    rm(l,dsel,d,pp,x,y);gc();
    return (list("mod"=mod,
                 "obs"=obs,
                 "ilab"=ilab,
                 "isel"=isel,
                 "valid"=valid,
                 "xl"=xl,
                 "yl"=yl,
                 "setl"=setl,
                 "il"=il,
                 "xr"=xr,
                 "yr"=yr,
                 "legs"=legs,
                 "cols"=cols,
                 "ltys"=ltys,
                 "lwds"=lwds,
                 "stats"=stats));
};
