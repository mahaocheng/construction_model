# TODO: Add comment
# 
# Author: lenovo
###############################################################################


#load libraries

library("plyr")
library('rJava')
#library('ggplot2')
#library('e1071')
library('XML')
library('rjson')

#data simulation
if(FALSE){
	Java2R_xml_after<-"<?xml version=\"1.0\" encoding=\"GBK\"?><TranData><BaseInfo><InsuID>0099</InsuID><BusiDate>20160713</BusiDate></BaseInfo><ContInfos><ContNums>1</ContNums><ContInfo><ContNum>1</ContNum><ContNo>C006841</ContNo><AppFlag>1</AppFlag><Appnt><AppntNo></AppntNo><AppntSex>M</AppntSex><AppntAppage>42</AppntAppage><AppntOccupationType>1</AppntOccupationType><AppntMarriage>1</AppntMarriage><AppntChild>0</AppntChild><AppntSmoke>1</AppntSmoke><RelaToInsured></RelaToInsured><AppntCompanyType>1</AppntCompanyType><AppntCompany>56</AppntCompany><AppntRetire>0</AppntRetire><AppntIncome>5600.00</AppntIncome><AppntFamilyIncome>6700.00</AppntFamilyIncome><AppntIncomeWay>1</AppntIncomeWay><AppntHeight>176</AppntHeight><AppntWeight>76</AppntWeight><AppntEducation>6</AppntEducation><HealthCTSum>0.00</HealthCTSum><HealthClaimSum>0.00</HealthClaimSum></Appnt><Insured><InsuredNo></InsuredNo><InsuredSex>M</InsuredSex><InsuredAppage>42</InsuredAppage><InsuredBirthday></InsuredBirthday><InsuredIDType>0</InsuredIDType><InsuredIDNo></InsuredIDNo><InsuredOccupationType></InsuredOccupationType><InsuredOccupationCode>0303006</InsuredOccupationCode><insuredNationality>CHN</insuredNationality><InsuredMarriage>1</InsuredMarriage><InsuredChild>0</InsuredChild><InsuredFat>1</InsuredFat><InsuredSmoke>8</InsuredSmoke><InsuredProvince></InsuredProvince><InsuredCountry>56</InsuredCountry><insuredCompany>0</insuredCompany><InsuredRetire>5</InsuredRetire><InsuredIncome>6700.00</InsuredIncome><InsuredFamilyIncome>1.00</InsuredFamilyIncome><InsuredIncomeWay>17</InsuredIncomeWay><InsuredHeight>76</InsuredHeight><InsuredWeight>6</InsuredWeight><InsuredEducation>1</InsuredEducation><InsuredMed>4</InsuredMed><HealthSumPrem>1.00</HealthSumPrem><HealthCount></HealthCount><DailyAmnt>6.50</DailyAmnt><CriticalAmnt>0.00</CriticalAmnt><MedicalAmnt>0.00</MedicalAmnt><TradeInsure>0</TradeInsure><AbnormalHReport>0</AbnormalHReport><InsuredPayoutRatio>11466.00</InsuredPayoutRatio></Insured><AgentInfo><AgentCode>111466</AgentCode><AgentSex>4</AgentSex><AgentAge></AgentAge><AgentBirthday>1</AgentBirthday><AgentMarriage>1</AgentMarriage><AgentChild>3</AgentChild><AgentClass></AgentClass><AgentSmoke></AgentSmoke><RelaToAppnt>86</RelaToAppnt><AgentComcode>11</AgentComcode><WorkYear>1</WorkYear><SaleChnl>11</SaleChnl><FYP>4235.69</FYP><FYC>0.89</FYC><PastCase>84.78</PastCase><PayoutRatio>11</PayoutRatio></AgentInfo></ContInfo></ContInfos><CaseInfo><CaseNo>4444</CaseNo><MedType>1</MedType><DiseaseClass>A00</DiseaseClass><DiseaseCode>444</DiseaseCode><FYAmnt>444.00</FYAmnt><ZJAmnt>444.00</ZJAmnt><JTAmnt>333.00</JTAmnt><DiseaseReason>2</DiseaseReason><AccAge>34</AccAge><AccDate>2016-06-29</AccDate><BusinesstoCase>3</BusinesstoCase><HospitalTime>3</HospitalTime><HospitalInfo><HospitalClass>2</HospitalClass><HospitalBody>1</HospitalBody><HospitalArea>1002</HospitalArea><HospitalType>0</HospitalType></HospitalInfo></CaseInfo></TranData>"
}
#doc<-xmlTreeParse(javaxml_after,encoding="GBK")

doc<-xmlTreeParse(Java2R_xml_after,encoding="GBK")
doc_list<-xmlToList(doc)
#doc_list$ContInfos$ContInfo$AgentInfo$SaleChnl


scoremax_flag<-c(5,6,7,5,6,7,5,4,6,5,4,5,5,4,6,4,5,6,5)
score_flag<-c()
for(i in 1:19)
{
	score_flag[i]<-sample(1:scoremax_flag[i],1)
}


con_after = xmlOutputDOM(tag="TranData", xmlDeclaration = 'version="1.0" encoding="GBK"')
con_after$addTag("BaseInfo", close=FALSE) 
con_after$addTag("BusiDate", doc_list$BaseInfo$BusiDate) 
con_after$addTag("InsuID", doc_list$BaseInfo$InsuID)
con_after$closeTag()
con_after$addTag("CaseInfo", close = FALSE) 
con_after$addTag("CaseNo", doc_list$CaseInfo$CaseNo)
con_after$addTag("CaseRate", "13") 
con_after$addTag("StateFlag", "1") 
con_after$addTag("FalseFlag", "2") 
con_after$addTag("FalseClass", "2") 
con_after$addTag("FrontFlag", "2")
con_after$addTag("DetailInfo", close = FALSE)
con_after$addTag("Flag01", as.character(score_flag[1]))
con_after$addTag("Flag02", as.character(score_flag[2])) 
con_after$addTag("Flag03", as.character(score_flag[3])) 
con_after$addTag("Flag04", as.character(score_flag[4])) 
con_after$addTag("Flag05", as.character(score_flag[5])) 
con_after$addTag("Flag06", as.character(score_flag[6])) 
con_after$addTag("Flag07", as.character(score_flag[7])) 
con_after$addTag("Flag08", as.character(score_flag[8])) 
con_after$addTag("Flag09", as.character(score_flag[9])) 
con_after$addTag("Flag10", as.character(score_flag[10])) 
con_after$addTag("Flag11", as.character(score_flag[11])) 
con_after$addTag("Flag12", as.character(score_flag[12])) 
con_after$addTag("Flag13", as.character(score_flag[13])) 
con_after$addTag("Flag14", as.character(score_flag[14])) 
con_after$addTag("Flag15", as.character(score_flag[15])) 
con_after$addTag("Flag16", as.character(score_flag[16])) 
con_after$addTag("Flag17", as.character(score_flag[17])) 
con_after$addTag("Flag18", as.character(score_flag[18])) 
con_after$addTag("Flag19", as.character(score_flag[19])) 
con_after$addTag("Flag20", "0") 
con_after$closeTag()		
con_after$closeTag()  
Return2Java_predict_after<-toString.XMLNode(con_after$value())
if(FALSE){
	doc_test<-xmlTreeParse(Return2Java_predict_after,encoding="GBK")
	doc_list<-xmlToList(doc_test)
}

if(FALSE){
	con_before<-xmlOutputDOM(tag="TranData", xmlDeclaration = 'version="1.0" encoding="GBK"')
	con_before$addTag("BaseInfo", close=FALSE) 
	con_before$addTag("BusiDate", doc_list$BaseInfo$BusiDate) 
	con_before$addTag("InsuID", doc_list$BaseInfo$InsuID)
	con_before$addTag("BatchNo", doc_list$BaseInfo$BatchNo)
	con_before$closeTag() 
	con_before$addTag("ContInfos", close = FALSE)
	con_before$addTag("ContNums", doc_list$ContInfos$ContNums) 
	con_before$addTag("ContInfo", close = FALSE)
	con_before$addTag("ContNo",  doc_list$ContInfos$ContInfo$ContNo) 
	con_before$addTag("CaseRate", " ***") 
	con_before$addTag("StateFlag", "0") 
	con_before$addTag("FalseFlag", "0") 
	con_before$addTag("FalseClass", "1") 
	con_before$addTag("FrontFlag", "2") 
	con_before$closeTag()
	con_before$closeTag()
	Return2Java_predict_after<-toString.XMLNode(con_before$value())
}




