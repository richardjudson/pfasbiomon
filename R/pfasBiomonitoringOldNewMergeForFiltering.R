#######################################################################################
#' Setup the new filtered input file using the latest assays from Doris
#'
#######################################################################################
pfasBiomonitoringOldNewMergeForFiltering <- function() {
  printCurrentFunction()
  cat("read files\n")
  dir = paste0("../data/")

  # file = paste0(dir,"ACToR biomon data/PFAS biomonitoring assays raw res_actor_2023q1 2022-12-21.xlsx")
  # assays2 = read.xlsx(file)
  #
  # file = paste0(dir,"ACToR biomon data/PFAS Blood assay master list 2022-11-30 Annotated.xlsx")
  # assays = read.xlsx(file)
  # assays2 = unique(assays2[,names(assays)])
  # for(i in 1:nrow(assays)) {
  #   if(assays[i,"assay_name"]=="file missing") {
  #     source = assays[i,"source"]
  #     if(is.element(source,assays2$source)) {
  #       temp = assays2[is.element(assays2$source,source),]
  #       assays[i,"assay_name"] = temp[1,"assay_name"]
  #       assays[i,"source_name_aid"] = temp[1,"source_name_aid"]
  #       assays[i,"a_description"] = temp[1,"a_description"]
  #       assays[i,"assay_id"] = temp[1,"assay_id"]
  #       assays[i,"url"] = temp[1,"url"]
  #       assays[i,"notes"] = temp[1,"notes"]
  #     }
  #     else cat("source missing",source,"\n")
  #   }
  # }
  # slist1 = unique(assays$source)
  # slist2 = unique(assays2$source)
  # slist3 = slist2[!is.element(slist2,slist1)]
  # assays3 = assays2[is.element(assays2$source,slist3),]
  # assays = rbind(assays,assays3)
  # browser()
  # file = paste0(dir,"ACToR biomon data/PFAS Blood assay master list 2022-12-22 Annotated.xlsx")
  # write.xlsx(assays,file)

  file = paste0(dir,"ACToR biomon data/PFAS Blood assay master list 2022-12-22 Annotated nrow edited.xlsx")
  print(file)
  assays = read.xlsx(file)
  assays$nrow = 0
  assays = assays[is.element(assays$useme,"yes"),]
  slist = unique(assays$source_name_aid)
  #browser()

  file = paste0(dir,"ACToR biomon data/actor pfas biomonitoring filtered 2022-12-01.xlsx")
  print(file)
  #dtxsid casrn	name	aid	acid	snaid	acname	desc	qc	matrix	metric	value	units	units_original	result_group	notes
  mat = read.xlsx(file)
  mat$value_string = NA
  print(dim(mat))
  file = paste0(dir,"ACToR biomon data/PFAS biomonitoring data raw res_actor_2023q1 2023-01-04.xlsx")
  print(file)
#                                       PFAS biomonitoring data raw res_actor_2023q1 2023-01-04
  mat2 = read.xlsx(file)
  print(dim(mat2))

  nlist = c("dtxsid","casrn","name","assay_id","assay_component_id","source_name_aid",
            "assay_component_name","ac_description", "concentration","concentration_string",
            "units", "result_group","notes")

  mat2 = mat2[,nlist]
  nlist = c("dtxsid","casrn","name","aid","acid","snaid",
            "acname","desc", "value","value_string",
            "units", "result_group","notes")
  names(mat2) = nlist
  mat2$matrix = NA
  mat2$metric = NA
  mat2$units_original = mat2$units
  mat2$qc = NA

  mat2 = mat2[,names(mat)]
  aclist = unique(mat$acid)
  mat3 = mat2[!is.element(mat2$acid,aclist),]
  #browser()
  mat$useme = 1
  mat3$useme = NA
  mat4 = rbind(mat,mat3)
  nlist = c("dtxsid","casrn","name","aid","acid","snaid","acname",
            "desc","useme","qc","matrix","metric","value","units","units_original",
            "result_group","notes","value_string")
  mat4 = mat4[,nlist]

  alist = unique(assays$assay_id)
  alist = alist[!is.element(alist,mat4$aid)]
  cat("length of missing assays:",length(alist),"\n")

  for(i in 1:nrow(assays)) {
    aid = assays[i,"assay_id"]
    if(!is.na(aid)) {
      temp = mat4[mat4$aid==aid,]
      assays[i,"nrow"] = nrow(temp)
      cat(assays[i,"source"],assays[i,"nrow"],"\n")
    }
  }
  file = paste0(dir,"ACToR biomon data/PFAS Blood assay master list 2023-01-04 Annotated nrow.xlsx")
  write.xlsx(assays,file)
  cat(nrow(mat4),"\n")
  mat4 = mat4[is.element(mat4$snaid,slist),]
  cat(nrow(mat4),"\n")

  file = paste0(dir,"ACToR biomon data/actor pfas biomonitoring filtered 2023-01-04.xlsx")
  write.xlsx(mat4,file)
}
