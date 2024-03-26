#-------------------------------------------------------------------------------
#' Parse the ACToR files and fill in the remaining missed data
#' @param dir The directory where the lists are stored
#-------------------------------------------------------------------------------
ACToR.parse <- function(dataset="PFAS_3M") {
  printCurrentFunction()
  dir = paste0("data/ACToR/",dataset,"/")
  file = paste0(dir,"Bioassay/AssayDescriptions.txt")
  nlist = c("snaid","aname","adesc","url",
            "acid","acname","acdesc","acunit","acvtype","notes")
  row = as.data.frame(matrix(nrow=1,ncol=length(nlist)))
  names(row) = nlist
  assays = NULL
  mat = read.delim(file, header = F, sep = "\t")

  snaid = NA
  aname = NA
  adesc = NA
  url = NA
  acid = NA
  acname = NA
  acdesc = NA
  acunit = NA
  acvtype = NA
  notes = ""

  for(i in 1:nrow(mat)) {
    tag = mat[i,1]
    val = mat[i,2]
    if(tag=="SOURCE_NAME_AID") snaid = val
    if(tag=="ASSAY_NAME") aname = val
    if(tag=="ASSAY_DESCRIPTION") adesc = val
    if(tag=="ASSAY_URL") url = val
    if(tag=="ASSAY_NOTE") notes = paste0(notes,"|",val)

    if(tag=="ASSAY_COMP_ID") {
      if(!is.na(acid)) {
        row["snaid"] = snaid
        row["aname"] = aname
        row["adesc"] = adesc
        row["url"] = url
        row["notes"] = notes
        row["acid"] = acid
        row["acname"] = acname
        row["acdesc"] = acdesc
        row["acunit"] = acunit
        row["acvtype"] = acvtype
        assays = rbind(assays,row)
        row[] = NA
        acid = NA
        acname = NA
        acdesc = NA
        acunit = NA
        acvtype = NA
      }
      acid = val
    }
    if(tag=="ASSAY_COMP_NAME") acname = val
    if(tag=="ASSAY_COMP_DESC") acdesc = val
    if(tag=="ASSAY_COMP_VALUE_TYPE") acvtype = val
    if(tag=="ASSAY_COMP_UNIT") acunit = val
     if(tag=="END") {
       row["snaid"] = snaid
       row["aname"] = aname
       row["adesc"] = adesc
       row["url"] = url
       row["notes"] = notes
       row["acid"] = acid
       row["acname"] = acname
       row["acdesc"] = acdesc
       row["acunit"] = acunit
       row["acvtype"] = acvtype
       assays = rbind(assays,row)
       row[] = NA
       acid = NA
       acname = NA
       acdesc = NA
       acunit = NA
       acvtype = NA

       snaid = NA
       aname = NA
       adesc = NA
       url = NA
       notes = ""
     }
  }

  temp = unique(assays[,c("snaid","aname","adesc","url","notes")])
  names(temp) = c("source_name_aid","assay_name","a_description","url","notes")
  temp$source = dataset
  temp$location = NA
  temp$population = NA
  temp$assay_id = NA
  temp$useme = NA
  temp$nrow = 0
  nlist = c("source","assay_name","location","population","source_name_aid",
            "a_description","assay_id","url","notes","nrow","useme")
  amat = temp[,nlist]
  #amat = amat[order(amat$source_name_aid),]
  if(!exists("CATALOG")) {
    file = "data/pfas_catalog 2021-10-06.xlsx"
    catalog = read.xlsx(file)
    CATALOG  <<- catalog
  }

  if(!exists("DATA0")) {
    file = "data/actor pfas biomonitoring filtered 2023-01-09.xlsx"
    data0 = read.xlsx(file)
    DATA0 <<- data0
  }
  data0 = DATA0
  nlist = names(data0)
  row = as.data.frame(matrix(nrow=1,ncol=length(nlist)))
  names(row) = nlist
  data = NULL
  for(i in 1:nrow(amat)) {
    snaid = amat[i,"source_name_aid"]
    cat(snaid,"\n")
    notes = amat[i,"notes"]
    file = paste0(dir,"Bioassay/",snaid,".xlsx")
    print(file)
    ba = read.xlsx(file)
    snaid_short = substr(snaid,1,(nchar(snaid)-6))
    file = paste0(dir,"Substance/",snaid_short,"_Substance.xlsx")
    print(file)
    sa = read.xlsx(file)
    atemp = assays[is.element(assays$snaid,snaid),]
    atemp$acid = as.character(atemp$acid)
    for(j in 1:nrow(ba)) {
      snsid = ba[j,"SOURCE_NAME_SID"]
      cat(snaid,snsid,"\n")
      #if(snaid=="PFAS_3M_27544_Decatur_Demogr_1998_AID_1") browser()
      if(is.element(snsid,sa$SOURCE_NAME_SID)) {
        casrn = sa[is.element(sa$SOURCE_NAME_SID,snsid),"CASRN"]
        dtxsid = CATALOG[is.element(CATALOG$casrn,casrn),"dtxsid"]
        name = CATALOG[is.element(CATALOG$casrn,casrn),"name"]

        if(sum(nchar(dtxsid))==0) dtxsid = "NODTXSID"
        if(sum(nchar(name))==0) name = "NONAME"
        for(k in 2:ncol(ba)) {
          acid = names(ba)[k]
          acname = atemp[is.element(atemp$acid,acid),"acname"]
          acdesc = atemp[is.element(atemp$acid,acid),"acdesc"]
          acunit = atemp[is.element(atemp$acid,acid),"acunit"]
          acvtype = atemp[is.element(atemp$acid,acid),"acvtype"]

  cat(i,j,k,acid,acname,"[",casrn,"]",class(casrn),acname,"\n")
  #if(snaid=="PFAS_3M_29627_Pilot_Child_1999_AID_1") browser()
  if(is.null(casrn)) browser()
          row[1,"dtxsid"]= dtxsid
          row[1,"casrn"]= casrn
          row[1,"name"]= name
          row[1,"aid"]= NA
          row[1,"acid"]= acid
          row[1,"snaid"]= snaid
          row[1,"acname"]= acname
          row[1,"desc"]= acdesc
          row[1,"useme"]= 2
          row[1,"qc"]= NA
          row[1,"matrix"]= NA
          row[1,"metric"]= NA
          row[1,"value"]= ba[j,k]
          row[1,"units"]= acunit
          row[1,"units_original"]= acunit
          row[1,"result_group"]= j
          row[1,"notes"]= notes
          row[1,"value_string"]= ba[j,k]
          data = rbind(data,row)
        }
      }
    }
  }

  file = paste0("data/",dataset," assasys.xlsx")
  write.xlsx(amat,file)
  file = paste0("data/",dataset," data.xlsx")
  write.xlsx(data,file)
}
