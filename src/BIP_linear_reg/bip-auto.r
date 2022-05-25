# This script does what???
# packages
library(usmap)

# get the list of states served by the program: column "States Served" from project spreadsheet:
# load in project data spreadsheet
bip_obl = readxl::read_xlsx("BIP_linear_reg/BIP_R1-R2AwardeeMaster_10-29-10_updNetObl_7-3-19 (1).xlsx", sheet = 2)
# get the list of BIP program names
bip_rusid = apply(bip_obl[,c(1,2)], 1, function(x) paste(x[1], x[2], sep="-"))

# initilize the empty list 
states_served = list()
# if there is only one state teh lengthof the column will be equal 2,
# but if the program, spans over multiple states separarte the state vector  
for(i in 1:nrow(bip_obl[,11])){
  if(nchar(bip_obl[i,11]) == 2){
    states_served[[i]] = as.character(bip_obl[i,11])
  }else{
    sts = seq(1, as.numeric(nchar(bip_obl[i,11])), by = 9)
    states = c()
    for(j in 1:length(sts)){
      states = c(states, substr(bip_obl[i,11], start = sts[j], stop = sts[j]+1))
    }
    states_served[[i]] = as.character(states)
  }
}
# get sates 2-digit fips codes and full names (uses 'usmap' package) 
fips_state = lapply(states_served, fips)
names_state = lapply(states_served, function(x){
  if(length(x) == 1) state.name[grep(x, state.abb)]
  else{
    as.vector(sapply(x, function(y) state.name[grep(y, state.abb)]))
  }
})

# file.list = list.files("~/")[11:142]
# bip_name = na.exclude(sapply(file.list, function(x){
#   strsplit(x, split = " ")[[1]][2]
# }))
# bip_name[length(bip_name)] = "VA1108-B39"
# bip.names = unique(as.vector(unlist(sapply(bip_name, function(x){
#   if(nchar(x) == 6) return(x)
#   else if(nchar(x) == 10) return(substr(x,start = 1, stop = 6))
#   else if(nchar(x) == 7) return(substr(x,start = 2, stop = 7))
# }))))
# bip.names[match(as.matrix(bip_obl[,1]),bip.names)]
