#
# Comment arrays: attributes legends columns
#

getArguments <- function () {
   args = commandArgs(trailingOnly=TRUE);
   if (length(args)==0) {
      stop("At least one argument must be supplied (input file).n", call.=FALSE);
   } else if (length(args)==1) {
     # default output file
     args[2] = "out.ps";
   };
   return (args);
}

readComments <- function (filename,name) {
   con = file(args[1], "r");
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

## function to open graphics output
openPlot <- function(filename,attributes) {
  format=getFormat(attributes);
  orientation=getOrientation(attributes);
  if (identical(format,"Jpeg")) {
   print("Opening jpeg.");
   jpeg(filename);
  } else if (identical(format,"png")) {
   print("Opening png.");
   png(filename);
  } else if (identical(format,"PDF")) {
   print("Opening PDF.");
   pdf(filename);
  } else if (identical(format,"PostScript")) {
   print("Opening PostScript.");
   postscript(filename,horizontal=orientation);
  } else {
   print("Unknown graphics type.");
   print(format);
  };
};

closePlot <- function() {
  dev.off();
}

# pch = points type
# lty = line type

scatterPlot <- function(filename,attr,legs,data,title,setcol,xcol,ycol,xlab,ylab) {
   x <- data[,get(xcol)];
   y <- data[,get(ycol)];
  openPlot(filename,attr);
  plot(x,y);
  closePlot();
};

seriesPlot <- function(filename,attr,legs,data,title,setcol,timecol,modcol,obscol,lab) {
   t <- data[,get(timecol)];
   m <- data[,get(modcol)];
   o <- data[,get(obscol)];
  openPlot(filename,attr);
  plot(t,o);
  closePlot();
};


scorePlot <- function(filename,attr,legs,data,title,setcol,refcol,timecol,modcol,obscol,lab) {
   r <- data[,get(recol)];
   t <- data[,get(timecol)];
   m <- data[,get(modcol)];
   o <- data[,get(obscol)];
  openPlot(filename,attr);
  plot(t-r,m-o);
  closePlot();
};


histPlot <- function(filename,attr,legs,data,title,setcol,refcol,timecol,modcol,obscol,lab) {
   r <- data[,get(recol)];
   t <- data[,get(timecol)];
   m <- data[,get(modcol)];
   o <- data[,get(obscol)];
  openPlot(filename,attr);
  hist(m-o);
  closePlot();
};

