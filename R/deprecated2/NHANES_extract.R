#'
#' Code to format the NHANES data
#' Note that this will only run under R 4.x
#'
library(foreign)
library(openxlsx)
library(stringr)
extract_NHANES <- function(dir="../data/",
                            dataset="PFAS_J") {

  mat = read.xport(paste0(dir,"/NHANES/",dataset,".xpt"))
  file = paste0(dir,"NHANES/NHANES Legend.xlsx")
  print(file)
  legend = read.xlsx(file)

  nchem = nrow(legend)
  res = NULL
  name.list = c("row","study_code","dtxsid","name","casrn","source",
                "assay_id","assay_component_id","assay_name","assay_component_name",
                "population","assay_category","value_numerical","units",
                "substance_id","result_group")
  row = as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(row) = name.list
  for(i in 1:nrow(legend)) {
    varname = legend[i,"varname"]
    varnameL = paste0(varname,"L")
    varnameL = str_replace(varnameL,"X","D")
    row[1,"dtxsid"] = legend[i,"dtxsid"]
    row[1,"name"] = legend[i,"name"]
    row[1,"casrn"] = legend[i,"casrn"]
    row[1,"study_code"] = "NHANES"
    row[1,"source"] = "NHANES"
    row[1,"assay_id"] = -1
    row[1,"assay_component_id"] = -1
    row[1,"assay_name"] = "NHANES"
    row[1,"substance_id"] = -1
    row[1,"result_group"] = -1
    row[1,"population"] = "NHANES"
    row[1,"assay_category"] = "Exposure; Biomonitoring; Blood"
    row[1,"units"] = "ng/mL"

    x = mat[,varname]
    y = mat[,varnameL]
    x = x[!is.na(x)]
    y = y[!is.na(y)]

    # LOD
    row[1,"assay_component_name"] = "LOD"
    row[1,"value_numerical"] = 0.1
    row[1,"units"] = "ng/mL"
    res = rbind(res,row)

    #n samples
    row[1,"assay_component_name"] = "n samples"
    row[1,"value_numerical"] = length(x)
    row[1,"units"] = "number"
    res = rbind(res,row)

    #n detected
    row[1,"assay_component_name"] = "n detected"
    row[1,"value_numerical"] = length(y[y==0])
    row[1,"units"] = "number"
    res = rbind(res,row)

    #%<LOD
    row[1,"assay_component_name"] = "%<LOD"
    row[1,"value_numerical"] = 100*length(y[y==1])/length(y)
    row[1,"units"] = "%"
    res = rbind(res,row)

    #%<LOD
    row[1,"assay_component_name"] = "%>LOD"
    row[1,"value_numerical"] = 100*length(y[y==0])/length(y)
    row[1,"units"] = "%"
    res = rbind(res,row)

    q = quantile(x,probs=seq(0,1,0.05))

    row[1,"assay_component_name"] = "05th percentile"
    row[1,"value_numerical"] = q[2]
    row[1,"units"] = "ng/mL"
    res = rbind(res,row)

     row[1,"assay_component_name"] = "10th percentile"
    row[1,"value_numerical"] = q[3]
    row[1,"units"] = "ng/mL"
    res = rbind(res,row)

    row[1,"assay_component_name"] = "25th percentile"
    row[1,"value_numerical"] = q[6]
    row[1,"units"] = "ng/mL"
    res = rbind(res,row)

    row[1,"assay_component_name"] = "50th percentile"
    row[1,"value_numerical"] = q[11]
    row[1,"units"] = "ng/mL"
    res = rbind(res,row)

    row[1,"assay_component_name"] = "75th percentile"
    row[1,"value_numerical"] = q[16]
    row[1,"units"] = "ng/mL"
    res = rbind(res,row)

    row[1,"assay_component_name"] = "90th percentile"
    row[1,"value_numerical"] = q[19]
    row[1,"units"] = "ng/mL"
    res = rbind(res,row)

    row[1,"assay_component_name"] = "95th percentile"
    row[1,"value_numerical"] = q[20]
    row[1,"units"] = "ng/mL"
    res = rbind(res,row)

    row[1,"assay_component_name"] = "maximum"
    row[1,"value_numerical"] = max(x)
    row[1,"units"] = "ng/mL"
    res = rbind(res,row)

    row[1,"assay_component_name"] = "minimum"
    row[1,"value_numerical"] = min(x)
    row[1,"units"] = "ng/mL"
    res = rbind(res,row)

    row[1,"assay_component_name"] = "median"
    row[1,"value_numerical"] = median(x)
    row[1,"units"] = "ng/mL"
    res = rbind(res,row)

    row[1,"assay_component_name"] = "mean"
    row[1,"value_numerical"] = mean(x)
    row[1,"units"] = "ng/mL"
    res = rbind(res,row)
  }
  file = paste0(dir,"NHANES values.xlsx")
  write.xlsx(res,file)
}
