#######################################################################################
#' Merge the ACToR data with NHANES and put in a common format
#'
#######################################################################################
pfasBiomonitoringMerge <- function() {
  printCurrentFunction()
  cat("read files\n")
  dir = paste0("data/")
  file = paste0(dir,"actor pfas biomonitoring filtered 2023-01-09.xlsx")
  print(file)
  mat = read.xlsx(file)

  file = paste0(dir,"3M final/PFAS_3M data 2023-01-11_Final.xlsx")
  print(file)
  mat2 = read.xlsx(file)
  mat2 = mat2[,names(mat)]

  file = paste0(dir,"all dataset data 2023-01-13.xlsx")
  print(file)
  mat3 = read.xlsx(file)
  mat = rbind(mat,mat2,mat3)

  mat = mat[mat$qc=="pass",]
  mat = mat[mat$useme>=1,]
  mlist = c("blood spot","cord blood","cord plasma",
              "cord serum","plasma","serum","whole blood")
  mat = mat[is.element(mat$matrix,mlist),]
  mat = mat[mat$value>0,]
  snaid.bad = NA
  mat = mat[!is.element(mat$snaid,snaid.bad),]
  mlist = c("01th percentile","05th percentile","10th percentile",
            "25th percentile","50th percentile","75th percentile",
            "90th percentile","95th percenile","95th percentile",
            "98th percentile","99th percentile",
            "maximum","mean","median","minimum",
            "concentration","LOD","LOQ")
  mat = mat[is.element(mat$metric,mlist),]
  mat$value = as.numeric(mat$value)
  #ulist = c("ug/L","ng/mL","ng/L","pg/mL","ng/g")
  ulist = c("ng/mL","ng/L","pg/mL")
  mat = mat[is.element(mat$units,ulist),]

  file = paste0(dir,"PFAS Blood assay master list 2023-01-11 FINAL.xlsx")
  print(file)
  assays = read.xlsx(file)

  file = paste0(dir,"PFAS_3M assasys 2021-01-11.xlsx")
  print(file)
  assays2 = read.xlsx(file)
  assays = rbind(assays,assays2)

  cat("check the snaids\n")
  slist = unique(mat$snaid)
  slist = slist[!is.element(slist,assays$source_name_aid)]
  if(length(slist)>0) {
    print(slist)
    browser()
  }

  cat("add pop and loc\n")
  nlist = c("dtxsid","casrn","name","snaid","acname","desc","matrix","metric","value","units","useme")
  mat = unique(mat[,nlist])
  mat$assay_name = NA
  mat$url = NA
  mat$location = NA
  mat$population = NA
  mat$source = NA
  snaid.list = unique(mat$snaid)
  for(snaid in snaid.list) {
    pop = assays[is.element(assays$source_name_aid,snaid),"population"]
    loc = assays[is.element(assays$source_name_aid,snaid),"location"]
    an = assays[is.element(assays$source_name_aid,snaid),"assay_name"]
    src = assays[is.element(assays$source_name_aid,snaid),"source"]
    url = assays[is.element(assays$source_name_aid,snaid),"url"]
    mat[is.element(mat$snaid,snaid),"population"] = pop
    mat[is.element(mat$snaid,snaid),"location"] = loc
    mat[is.element(mat$snaid,snaid),"assay_name"] = an
    mat[is.element(mat$snaid,snaid),"source"] = src
    mat[is.element(mat$snaid,snaid),"url"] = url
  }
  mat$value_original = mat$value
  mat$units_original = mat$units

  cat("fix units\n")
  for(i in 1:nrow(mat)) {
    unit = mat[i,"units"]
    val = as.numeric(mat[i,"value"])
    #if(unit=="ng/g") val = val*1
    #if(unit=="ug/L") val = val*1
    #if(unit=="ng/L") val = val*0.001
    if(unit=="pg/mL") val = val*0.001
    if(unit=="ng/mL") val = val
    if(unit=="ug/mL") val = val*1000
    mat[i,"value"] = as.numeric(val)
    mat[i,"units"] = "ng/mL"
  }

  cat("add NHANES\n")
  #"dtxsid"         "casrn"          "name"           "snaid"          "acname"         "desc"           "matrix"         "metric"         "value"          "units"
  # "location"       "population"     "value_original" "units_original"
  file = paste0(dir,"NHANES values.xlsx")
  #row	study_code		source	assay_id	assay_component_id	assay_name	assay_component_name	population	assay_category	value_numerical	units	substance_id	result_group
  print(file)
  nhanes = read.xlsx(file)
  nlist = c("dtxsid","casrn","name","assay_component_name",
            "population","value_numerical","units","source")
  nhanes = nhanes[,nlist]
  nhanes$location = "USA"
  nhanes$population = "general population"
  nhanes = nhanes[is.element(nhanes$units,"ng/mL"),]
  nhanes$value_original = nhanes$value
  nhanes$units_original = nhanes$units

  nlist = c("dtxsid","casrn","name","acname","population","value","units",
             "source","location","value_original","units_original")
  names(nhanes) = nlist
  nhanes$metric = nhanes$acname
  nhanes$assay_name = "NHANES"
  nhanes$snaid = "NHANES"
  nhanes$desc = "NHANES"
  nhanes$matrix = "serum"
  nhanes$url = "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/SSPFAS_J.htm"
  nhanes$useme = 1
  nhanes = nhanes[,names(mat)]
  mat = rbind(mat,nhanes)

  cat("add nicknames\n")
  mat$nickname = mat$name
  file = paste0(dir,"PFAS synonyms.xlsx")
  print(file)
  synonyms = read.xlsx(file)
  rownames(synonyms) = synonyms$dtxsid
  mat$nickname = mat$name
  dlist = unique(mat$dtxsid)
  for(dtxsid in dlist) {
    if(is.element(dtxsid,synonyms$dtxsid)) {
      nn = synonyms[dtxsid,"nickname"]
      mat[is.element(mat$dtxsid,dtxsid),"nickname"] = nn
    }
  }
  nlist = c("dtxsid","casrn","name","nickname",
            "matrix","metric","value","units",
            "source", "location","population",
            "assay_name","snaid","acname","desc","url",
            "value_original","units_original","useme")
  mat = mat[,nlist]
  mat$exposed = "Unclassified"
  mat$metric_class = NA
  for(i in 1:nrow(mat)) {
    pop = mat[i,"population"]
    if(!is.na(pop)) {
      if(grepl("exposed",pop,ignore.case=T)) mat[i,"exposed"] = "Exposed"
      else mat[i,"exposed"] = "General"
    }
    metric = mat[i,"metric"]
    if(metric=="maximum") mc = "High"
    if(metric=="mean") mc = "50th percentile"
    if(metric=="median") mc = "50th percentile"
    if(metric=="minimum") mc = "Low"
    if(metric=="95th percentile") mc = "High"
    if(metric=="10th percentile") mc = "Low"
    if(metric=="LOD") mc = "LOD"
    if(metric=="75th percentile") mc = "75th percentile"
    if(metric=="90th percentile") mc = "High"
    if(metric=="05th percentile") mc = "Low"
    if(metric=="50th percentile") mc = "50th percentile"
    if(metric=="25th percentile") mc = "25th percentile"
    if(metric=="LOQ") mc = "LOQ"
    if(metric=="99th percentile") mc = "High"
    if(metric=="98th percentile") mc = "High"
    if(metric=="01th percentile") mc = "Low"
    mat[i,"metric_class"] = mc

  }
  file = paste0(dir,"PFAS biomonitoring data final.xlsx")
  write.xlsx(mat,file)
  file = paste0(dir,"PFAS biomonitoring assays final.xlsx")
  write.xlsx(assays,file)
}
