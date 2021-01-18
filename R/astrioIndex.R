astrioIndex <- function(ff, out_file = TRUE, out_name="Indexed_file.fcs"){
  rows<-1:nrow(ff)
  XYindex<-matrix(nrow = nrow(ff), ncol = 2)
  colnames(XYindex)<-c("X-index","Y-index")

  a<-1:6
  b<-7:32
  zero<-raw(32)

  for(m in rows){
    #Reset X1 and Y1 values
    X1<-raw(1)
    Y1<-raw(1)

    # Extract m-th "Sort Classifier" value
    sortcl<-exprs(ff)[m,'Sort Classifier']
    bit<-intToBits(sortcl)

    # Extract the X-parameter bits
    for(i in a){
      X1[i]<-bit[20+i]
    }

    # Add 26 zeroes to complete binary
    for(n in b){
      X1[n]<-zero[n]
    }

    # Invert X1 and convert to integer
    rev(X1)
    X<-packBits(X1, "integer")

    # Extract the Y-parameter bits
    for(i in a){
      Y1[i]<-bit[26+i]
    }

    # Add 26 zeroes to complete binary
    for(n in b){
      Y1[n]<-zero[n]
    }

    # Invert Y1 and convert to integer
    rev(Y1)
    Y<-packBits(Y1, "integer")

    XYindex[m,"X-index"]<-X
    XYindex[m,"Y-index"]<-Y

  }
  fcs2 <- fr_append_cols(ff, XYindex)
  fcs2@description$`$FIL`<-paste0("indexed_", fcs2@description$`$FIL`)
  fcs2@description$FILENAME<-paste0("indexed_", fcs2@description$FILENAME)
  if(out_file==TRUE){
    write.FCS(fcs2, filename = as.character(out_name))
  }
  else {
    return(fcs2)
  }
  cat("Indexed!\n")
}
