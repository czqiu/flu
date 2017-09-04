###################################################################
#
# This is toconvert the raw symptom dataset into one that is more formated 
####################################################################

# read in the data set
setwd("/Users/chenyiy/Documents/中医网站2017/DLexplore/")
symptom <- read.csv("fluyianforRsheet1V1.csv",header=T,sep=",", stringsAsFactors = FALSE)

# We only care for 初诊at the moment. Remove all those that is not 初诊

symptom1 <- symptom[which(symptom$诊次=='初诊'),]

id.unique <- unique(symptom1$病人号)

# create an empty dataset

symp.new <- data.frame(病人号=character(),
                          发烧=character(), 
                          发烧长短=character(),
                          午后身热=character(),
                          怕冷=character(),
                          怕风=character(),
                          出汗=character(),
                          咳嗽=character(),
                          喘=character(),
                          胸闷=character(),
                          咽肿=character(),
                          咽痛=character(),
                          咽干=character(),
                          口渴=character(),
                          舌体=character(),
                          舌质=character(),
                          舌面=character(),
                          舌苔=character(),
                          痰量=character(),
                          痰色=character(),
                          痰中带血=character(),
                          饮水=character(),
                          躁烦=character(),
                          睡眠=character(),
                          神智不清=character(),
                          头晕=character(),
                          困倦=character(),
                          日夜差=character(),
                          小便=character(),
                          大便=character(),
                          胃口=character(),
                          恶心=character(),
                          呕吐=character(),
                          腹胀=character(),
                          腹拒按=character(),
                          脸红眼红唇干=character(),
                          胸闷=character(),
                          抽搐=character(),
                          惊悸=character(),
                          口苦=character(),
                          手脚四肢冷=character(),
                          后背冷=character(),
                          手抖=character(),
                          表证=character(),
                          头痛=character(),
                          胸痛=character(),
                          胁痛=character(),
                          身痛=character(),
                          胃痛=character(),
                          项背酸痛=character(),
                          鼻塞=character(),
                          鼻涕=character(),
                          打喷嚏=character(),
                          经常感冒=character(),
                          体形=character(),
                          淋巴结肿大=character(),
                          stringsAsFactors=FALSE) 


# define the array of symptons for each patient

发烧1 = c("不发烧","不发热")
发烧2 = c("发烧","发热","一般发烧","低热","低烧")
发烧3 = c("高热","高烧")
发烧4 = c("体温波动","往来寒热","高烧","先发热再发冷")

发烧op= c("不发烧","一般发烧","先发冷再发热","往来寒热")

发烧长短1 = c("发烧一周内","发烧初起","发热1日","发热2日","发热3日","发热4日","发热5日","发热6日","发热7日")
发烧长短2 = c("发烧一月内")
发烧长短3 = c("发烧一月以上","长期发烧")

发烧长短op= c("发烧一周内","发烧一月内","发烧一月以上")

午后身热1 = c("午后身热","下午发热","下午发烧")

怕冷1 = c("一点不怕冷","不怕冷","不恶寒","无恶寒")
怕冷2 = c("有点怕冷","稍怕冷","略微怕冷","微恶寒","稍恶寒")
怕冷3 = c("怕冷","很怕冷","恶寒")

怕冷op= c("一点不怕冷","略微怕冷","怕冷")



#sapply(whatswrong, function (y) sapply(体温2, function (x) grepl(y,x)))
#sapply(whatswrong, function (y) grepl(y,体温2))


match.sympt = function (zz)
{
  fs1 = any(match(发烧1,zz)==1)
  fs2 = any(match(发烧2,zz)==1)
  fs3 = any(match(发烧3,zz)==1)
  fs4 = any(match(发烧4,zz)==1)
  fs = c(fs1, fs2, fs3, fs4)
  fs.df = all(is.na(fs))
  发烧 = ifelse(fs.df == "FALSE", 发烧op[which(fs == "TRUE")], ifelse(fs.df == "TRUE", 发烧op[1]))
  
  fscd1 = any(match(发烧长短1,zz))
  fscd2 = any(match(发烧长短2,zz))
  fscd3 = any(match(发烧长短3,zz))
  fscd = c(fscd1, fscd2, fscd3)
  fscd.df = all(is.na(fscd))
  发烧长短 = ifelse(fscd.df == "FALSE", 发烧长短op[which(fscd == "TRUE")], ifelse(fscd.df == "TRUE", "NA"))
  
  whsr1 = any(match(午后身热1,zz))
  whsr.df = all(is.na(whsr1))
  午后身热 = ifelse(whsr.df == "FALSE", "午后身热", ifelse(whsr.df == "TRUE", "NA"))
  
  pl1 = any(match(怕冷1,zz))
  pl2 = any(match(怕冷2,zz))
  pl3 = any(match(怕冷3,zz))
  pl = c(pl1, pl2, pl3)
  pl.df = all(is.na(pl))
  怕冷 = ifelse(pl.df == "FALSE", 怕冷op[which(pl == "TRUE")], ifelse(pl.df == "TRUE", "NA"))
  
  result = list(发烧, 发烧长短, 午后身热, 怕冷)
  return (result) 
}

for (i in 1:length(id.unique)){
  locationtag = which(symptom1$病人号==id.unique[i])
  whatswrong = symptom1$症状[locationtag]
  symplist = match.sympt(whatswrong)
  symp.new[i,1] = id.unique[i]
  symp.new[i,2] = symplist[1]
  symp.new[i,3] = symplist[2]
  symp.new[i,4] = symplist[3]
  symp.new[i,5] = symplist[4]
}