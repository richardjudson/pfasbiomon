#######################################################################################
#' get biomonitoring data from actor
#'
#######################################################################################
pfasBiomonitoringFilter <- function() {
  printCurrentFunction()
  dir = paste0("../data/")
  file = paste0(dir,"PFAS biomonitoring data human blood 2021-10-27.xlsx")
  mat = read.xlsx(file)
  mat = mat[mat$value_numeric>0,]
  acn = unique(mat$assay_component_name)
  file = paste0(dir,"PFAS synonyms.xlsx")
  synonyms = read.xlsx(file)
  rownames(synonyms) = synonyms$dtxsid

  name.list = c("study","dtxsid","casrn","name","nickname","population","type","value","units","value_original","units_original")
  row = as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(row) = name.list
  res = NULL

  # type.list = c("05th percentile","10th percentile" ,"25th percentile","50th percentile","75th percentile",
  #               "90th percentile" ,"95th percentile","95th percentile CI lower","95th percentile CI upper",
  #               "98th percentile","99th percentile","concentration","LOD","LOQ","maximum","mean",
  #               "mean population","median","minimum")
  # mat = mat[is.element(mat$assay_component_name,type.list),]
  unit.list.0 = c("mcg/L", "ng/L", "ng/mL", "ng/mL (ppb)", "ppb",  "ppm",  "ug/L")
  unit.list.1 = c("ng/mL", "ng/mL","ng/mL", "ng/mL",       "ng/mL","ng/mL","ng/mL")
  conv.list   = c(1,        0.001,  1,       1,             1,      1000,  1)
  mat = mat[is.element(mat$units,unit.list.0),]
  conv = cbind(unit.list.0, unit.list.1, conv.list)
  conv = as.data.frame(conv,stringsAsFactors=F)
  conv[,3] = as.numeric(conv[,3])
  names(conv) = c("units_original","units","factor")

  for(i in 1:nrow(mat)) {
    dtxsid = mat[i,"dtxsid"]
    row[1,"study"] = mat[i,"study_code"]
    row[1,"dtxsid"] = dtxsid
    row[1,"name"] = mat[i,"name"]
    row[1,"nickname"] = mat[i,"name"]
    nickname = NA
    if(is.element(dtxsid,synonyms$dtxsid)) {
      nickname = synonyms[dtxsid,"nickname"]
    }
    if(is.na(nickname)) nickname = ""
    row[1,"nickname"] = nickname
    row[1,"casrn"] = mat[i,"casrn"]
    row[1,"type"] = mat[i,"assay_component_name"]
    row[1,"population"] = mat[i,"population"]

    units0 = mat[i,"units"]
    value0 = mat[i,"value_numerical"]
    row[1,"value_original"] = value0
    row[1,"units_original"] = units0

    units = conv[is.element(conv$units_original,units0),"units"]
    factor = conv[is.element(conv$units_original,units0),"factor"]
    value = value0*factor
    row[1,"value"] = value
    row[1,"units"] = units
    res= rbind(res,row)
  }
  file = paste0(dir,"PFAS biomonitoring data final.xlsx")
  res = unique(res)
  write.xlsx(res,file)
}
