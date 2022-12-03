NCBI <- function(x) {
  library(RCurl)
  library(stringr)
  library(XML)
  library(clusterProfiler)
  genes <- bitr(genes$symbol, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")
  genes$NCBI_url <- paste("https://www.ncbi.nlm.nih.gov/gene/",genes$ENTREZID,sep="")
  head(genes)
  # Official Full name的xpath：//*[@id="summaryDl"]/dd[2]/text()
  # HGNC ID的xpath：//*[@id="summaryDl"]/dd[3]/a
  # Gene type的xpath：//*[@id="summaryDl"]/dd[5]/text()
  # Summary的xpath：//*[@id="summaryDl"]/dd[10]/text()

  # 根据xpath获取节点内容：
  getNodesTxt <- function(html_txt1,xpath_p){
    els1 = getNodeSet(html_txt1, xpath_p)
    # 获得Node的内容，并且去除空字符：
    els1_txt <- sapply(els1,xmlValue)[!(sapply(els1,xmlValue)=="")]
    # 去除\n：
    str_replace_all(els1_txt,"(\\n )+","")
  }

  # 处理节点格式，为character且长度为0的赋值为NA：
  dealNodeTxt <- function(NodeTxt){
    ifelse(is.character(NodeTxt)==T && length(NodeTxt)!=0 , NodeTxt , NA)
  }

  c <- 1 #无响应或请求超时初始值
  for(i in 1:nrow(genes)){
    temp <- try(getURL(genes[i,"NCBI_url"]),silent=FALSE) #判断 grtURL是否返回错误值
    if('try-error' %in% class(temp))
    {doc <-NULL
    cat('第',i,'个失败！\n')
    c <- c+1
    }else{
      doc <- temp
      cat('第',i,'个成功！\n')
      html_txt1 = htmlParse(doc, asText = TRUE)
      # 获得Locus tag：
      genes[i,"GeneType"] <- dealNodeTxt(getNodesTxt(html_txt1,'//*[@id="summaryDl"]/dd[5]/text()'))
      cat("写入GeneType\t")
      # 获得summary：
      genes[i,"Summary"] <- ifelse(length(getNodesTxt(html_txt1,'//*[@id="summaryDl"]/dd[10]/text()'))!=0,getNodesTxt(html_txt1,'//*[@id="summaryDl"]/dd[10]/text()'),NA)
      cat("写入Summary\n")
    }
    if(c==20){stop()} #若getURL错误20次，则停止运行
    Sys.sleep(1) #睡眠1秒
  }

}
