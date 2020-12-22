# bdm-book
1. 《商业数据挖掘》一书中涉及到的源码和数据集。该书网上已售。京东：https://item.jd.com/10022778213927.html 当当：http://product.dangdang.com/1743508956.html

2. 本书有一些错误需要修订，参看文件“错误勘正.docx”

3. 每个用户R中的设置不同，在运行我提供的程序时，如果读文本文件没有将字符类型（character）自动转换成factor类型，从而造成程序错误，建议在read.table， read.csv函数中加入参数stringsAsFactors =T

例如，df<-read.csv('data/house-votes-84.data',header=F,stringsAsFactors =T)

4. 本书用到的所有包做成了一个压缩文件，用户可以从百度网盘下载，并解压到自己电脑的c盘
网址：https://pan.baidu.com/s/1aQSesku7jk3OpykyQJlb2A
提取码：ldez
该包适合R 3.5版。更高版本的R，需要重新安装这些包。
