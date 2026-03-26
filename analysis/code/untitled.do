*--------------------------------------------------------------
* 0. Import
*--------------------------------------------------------------
import delimited using "/Users/ziwenzu/Downloads/articles.csv", ///
    clear ///
    varnames(1) ///
    bindquote(strict)

*--------------------------------------------------------------
* 1. Clean base variable
*--------------------------------------------------------------
drop if missing(category)

capture drop category_clean category_lower cat_cluster cat_cluster_name
gen strL category_clean = trim(category)
gen strL category_lower = lower(category_clean)

*--------------------------------------------------------------
* 2. 新建 cluster 变量（细分类）
*--------------------------------------------------------------
gen byte cat_cluster = .

* 编码：
*  1 公共服务 / 便民信息
*  2 城市形象 / 历史文化
*  3 应急管理 / 公共安全
*  4 意识形态 / 宣传教育
*  5 政策信息 / 政务公开
*  6 教育 / 医疗 / 养老 / 社会保障
*  7 人事任免 / 招聘
*  8 市场监管 / 执法 / 社会治理
*  9 农业 / 农村 / 乡村振兴
* 10 国际事务 / 对外合作
* 99 其他 / 不适用

*--------------------------------------------------------------
* 2.1 公共服务 / 便民信息（Cluster 1）
*--------------------------------------------------------------
replace cat_cluster = 1 if missing(cat_cluster) & strpos(category_clean,"公共服务")>0
replace cat_cluster = 1 if missing(cat_cluster) & strpos(category_clean,"公众服务")>0
replace cat_cluster = 1 if missing(cat_cluster) & strpos(category_clean,"服务信息")>0
replace cat_cluster = 1 if missing(cat_cluster) & strpos(category_clean,"便民")>0
replace cat_cluster = 1 if missing(cat_cluster) & strpos(category_clean,"教育医疗养老")>0
replace cat_cluster = 1 if missing(cat_cluster) & regexm(category_lower,"public service")

*--------------------------------------------------------------
* 2.2 城市形象 / 历史文化 / 活动推广（Cluster 2）
*--------------------------------------------------------------
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"城市形象")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"城乡形象")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"城镇形象")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"形象与文化活动")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"城市故事")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"地方故事")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"历史文化")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"历史与文化")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"历史与文化活动")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"历史上的今天")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"故事与文化活动")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"体育与文化活动")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"体育文化活动")>0
replace cat_cluster = 2 if missing(cat_cluster) & strpos(category_clean,"体育赛事与城市形象")>0

*--------------------------------------------------------------
* 2.3 应急管理 / 公共安全 / 风险沟通（Cluster 3）
*--------------------------------------------------------------
replace cat_cluster = 3 if missing(cat_cluster) & strpos(category_clean,"应急管理")>0
replace cat_cluster = 3 if missing(cat_cluster) & strpos(category_clean,"應急管理")>0
replace cat_cluster = 3 if missing(cat_cluster) & strpos(category_clean,"风险沟通")>0
replace cat_cluster = 3 if missing(cat_cluster) & strpos(category_clean,"风险溝通")>0
replace cat_cluster = 3 if missing(cat_cluster) & strpos(category_clean,"安全生产")>0
replace cat_cluster = 3 if missing(cat_cluster) & strpos(category_clean,"安全管理")>0
replace cat_cluster = 3 if missing(cat_cluster) & strpos(category_clean,"消防")>0
replace cat_cluster = 3 if missing(cat_cluster) & strpos(category_clean,"防汛")>0
replace cat_cluster = 3 if missing(cat_cluster) & strpos(category_clean,"防灾")>0
replace cat_cluster = 3 if missing(cat_cluster) & strpos(category_clean,"疫情防控")>0
replace cat_cluster = 3 if missing(cat_cluster) & strpos(category_clean,"卫生健康与公共安全")>0

*--------------------------------------------------------------
* 2.4 意识形态 / 宣传教育 / 思想工作（Cluster 4）
*--------------------------------------------------------------
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"意识形态")>0
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"意识形態")>0
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"意識形态")>0
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"意識形態")>0
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"思想意识")>0
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"思想文化")>0
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"党的意识形态")>0
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"党风廉政")>0
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"党纪监察")>0
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"文明实践")>0
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"宣传教育")>0
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"宣传与教育")>0
replace cat_cluster = 4 if missing(cat_cluster) & strpos(category_clean,"宣传与意识形态")>0

*--------------------------------------------------------------
* 2.5 教育 / 医疗 / 养老 / 社会保障（Cluster 6）
*--------------------------------------------------------------
replace cat_cluster = 6 if missing(cat_cluster) & strpos(category_clean,"教育医疗养老")>0
replace cat_cluster = 6 if missing(cat_cluster) & regexm(category_clean,"^教育与")
replace cat_cluster = 6 if missing(cat_cluster) & strpos(category_clean,"教育与")>0
replace cat_cluster = 6 if missing(cat_cluster) & strpos(category_clean,"教育")>0
replace cat_cluster = 6 if missing(cat_cluster) & strpos(category_clean,"健康")>0
replace cat_cluster = 6 if missing(cat_cluster) & strpos(category_clean,"医疗")>0
replace cat_cluster = 6 if missing(cat_cluster) & strpos(category_clean,"卫生健康")>0
replace cat_cluster = 6 if missing(cat_cluster) & regexm(category_lower,"health")
replace cat_cluster = 6 if missing(cat_cluster) & regexm(category_lower,"medical")

*--------------------------------------------------------------
* 2.6 人事任免 / 干部公示 / 招聘（Cluster 7）
*--------------------------------------------------------------
replace cat_cluster = 7 if missing(cat_cluster) & strpos(category_clean,"人事任免")>0
replace cat_cluster = 7 if missing(cat_cluster) & strpos(category_clean,"人事信息")>0
replace cat_cluster = 7 if missing(cat_cluster) & strpos(category_clean,"人事通报")>0
replace cat_cluster = 7 if missing(cat_cluster) & strpos(category_clean,"干部公示")>0
replace cat_cluster = 7 if missing(cat_cluster) & strpos(category_clean,"干部管理")>0
replace cat_cluster = 7 if missing(cat_cluster) & strpos(category_clean,"职称评审")>0
replace cat_cluster = 7 if missing(cat_cluster) & strpos(category_clean,"招聘")>0
replace cat_cluster = 7 if missing(cat_cluster) & strpos(category_clean,"招录")>0
replace cat_cluster = 7 if missing(cat_cluster) & strpos(category_clean,"选拔")>0
replace cat_cluster = 7 if missing(cat_cluster) & strpos(category_clean,"选调")>0

*--------------------------------------------------------------
* 2.7 市场监管 / 执法 / 社会治理 / 公安司法（Cluster 8）
*--------------------------------------------------------------
replace cat_cluster = 8 if missing(cat_cluster) & strpos(category_clean,"市场监管")>0
replace cat_cluster = 8 if missing(cat_cluster) & strpos(category_clean,"执法通报")>0
replace cat_cluster = 8 if missing(cat_cluster) & strpos(category_clean,"公安事务")>0
replace cat_cluster = 8 if missing(cat_cluster) & strpos(category_clean,"公安服务")>0
replace cat_cluster = 8 if missing(cat_cluster) & strpos(category_clean,"刑事审判")>0
replace cat_cluster = 8 if missing(cat_cluster) & strpos(category_clean,"司法与法律服务")>0
replace cat_cluster = 8 if missing(cat_cluster) & strpos(category_clean,"司法公正")>0
replace cat_cluster = 8 if missing(cat_cluster) & strpos(category_clean,"社会治理")>0
replace cat_cluster = 8 if missing(cat_cluster) & strpos(category_clean,"垃圾分类与执法通报")>0
replace cat_cluster = 8 if missing(cat_cluster) & strpos(category_clean,"食品药品")>0

*--------------------------------------------------------------
* 2.8 农业 / 农村发展 / 乡村振兴（Cluster 9）
*--------------------------------------------------------------
replace cat_cluster = 9 if missing(cat_cluster) & strpos(category_clean,"农业")>0
replace cat_cluster = 9 if missing(cat_cluster) & strpos(category_clean,"农村发展")>0
replace cat_cluster = 9 if missing(cat_cluster) & strpos(category_clean,"乡村振兴")>0
replace cat_cluster = 9 if missing(cat_cluster) & strpos(category_clean,"三农")>0

*--------------------------------------------------------------
* 2.9 国际事务 / 对外合作（Cluster 10）
*--------------------------------------------------------------
replace cat_cluster = 10 if missing(cat_cluster) & strpos(category_clean,"国际事务")>0
replace cat_cluster = 10 if missing(cat_cluster) & strpos(category_clean,"国际关系")>0
replace cat_cluster = 10 if missing(cat_cluster) & strpos(category_clean,"国际交流")>0
replace cat_cluster = 10 if missing(cat_cluster) & strpos(category_clean,"国际合作")>0
replace cat_cluster = 10 if missing(cat_cluster) & strpos(category_clean,"国际友好")>0
replace cat_cluster = 10 if missing(cat_cluster) & strpos(category_clean,"友好城市")>0
replace cat_cluster = 10 if missing(cat_cluster) & strpos(category_clean,"国际医疗合作")>0
replace cat_cluster = 10 if missing(cat_cluster) & regexm(category_lower,"international")
replace cat_cluster = 10 if missing(cat_cluster) & regexm(category_lower,"diplom")

*--------------------------------------------------------------
* 2.10 政策信息 / 政务公开（Cluster 5）
*--------------------------------------------------------------
replace cat_cluster = 5 if missing(cat_cluster) & strpos(category_clean,"政务公开")>0
replace cat_cluster = 5 if missing(cat_cluster) & strpos(category_clean,"政策与政务公开")>0
replace cat_cluster = 5 if missing(cat_cluster) & strpos(category_clean,"政府公告")>0
replace cat_cluster = 5 if missing(cat_cluster) & strpos(category_clean,"公开程序与政务公开")>0
replace cat_cluster = 5 if missing(cat_cluster) & regexm(category_lower,"government disclosure")
replace cat_cluster = 5 if missing(cat_cluster) & regexm(category_lower,"policy and government disclosure")

*--------------------------------------------------------------
* 2.11 其他 / 不适用（Cluster 99）
*--------------------------------------------------------------
replace cat_cluster = 99 if missing(cat_cluster)

*--------------------------------------------------------------
* 3. 标签（细分类）
*--------------------------------------------------------------
label define cat_cluster_lbl                  ///
    1  "公共服务 / Public service"           ///
    2  "城市形象 & 历史文化"                 ///
    3  "应急管理 & 公共安全"                 ///
    4  "意识形态 & 宣传教育"                 ///
    5  "政策信息 & 政务公开"                 ///
    6  "教育 / 医疗 / 养老 / 社会保障"       ///
    7  "人事任免 & 招聘"                     ///
    8  "市场监管 / 执法 / 社会治理"          ///
    9  "农业 / 农村发展 / 乡村振兴"          ///
    10 "国际事务 / 对外合作"                 ///
    99 "其他 / 不适用"

label values cat_cluster cat_cluster_lbl

gen str40 cat_cluster_name = ""

*==============================================================
* 4. 二次兜底：只对 cat_cluster==99 的观测重分类
*==============================================================

* 4.1 时政与领导活动 -> 意识形态 & 宣传教育 (4)
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"时政与领导活动")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"时政于领导活动")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"时政，与领导活动")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"以时政与领导活动")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"新时代与领导活动")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"时代与领导活动")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"时报与领导活动")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"神神秘秘与领导活动")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"领导活动")>0

* 4.2 社会保障 / 民生福利 -> 教育/医疗/养老/社保 (6)
replace cat_cluster = 6 if cat_cluster==99 & strpos(category_clean,"社会保障与公共福利")>0
replace cat_cluster = 6 if cat_cluster==99 & strpos(category_clean,"社会保障与公众福利")>0
replace cat_cluster = 6 if cat_cluster==99 & strpos(category_clean,"社会保护与公共福利")>0
replace cat_cluster = 6 if cat_cluster==99 & strpos(category_clean,"社交保障与公共福利")>0
replace cat_cluster = 6 if cat_cluster==99 & strpos(category_clean,"民生保障与公共福利")>0
replace cat_cluster = 6 if cat_cluster==99 & strpos(category_clean,"民生保障与社会发展")>0
replace cat_cluster = 6 if cat_cluster==99 & strpos(category_clean,"民生实事与公共福利")>0
replace cat_cluster = 6 if cat_cluster==99 & strpos(category_clean,"薪酬救助与公共福利")>0
replace cat_cluster = 6 if cat_cluster==99 & strpos(category_clean,"贫困人口报销政策")>0
replace cat_cluster = 6 if cat_cluster==99 & strpos(category_clean,"社会保障")>0

* 4.3 群众/大众/民族/社区动员 -> 宣传/动员 (4)
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"群众动员与社会参与")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"群眾動員與社會參與")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"群动员与社会参与")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"群体动员与社会参与")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"社会动员与社会参与")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"社会参与与群众动员")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"民族动员与社会参与")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"社区动员与社会参与")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"大众动员与社会参与")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"大众参与与社会动员")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"全民动员与社会参与")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"公众动员与社会参与")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"公众参与与社会活动")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"社区治理与社会参与")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"青年发展与社会参与")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"游客动员与社会参与")>0

* 4.4 经济与发展建设 / 财税金融 / 统计 -> 政策信息 & 发展 (5)
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"经济与发展建设")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"Economic and Development Construction")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"经济视与发展建设")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"鼓励经济与发展建设")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"认识变化与发展建设")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"社会经济与发展建设")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"创业与经济发展")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"商业与发展建设")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"金融与发展建设")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"金融与银行服务")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"金融监管与政策解读")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"税务与财经信息")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"财政与经济信息")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"财政预算")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"统计数据")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"统计数据与工作汇报")>0
replace cat_cluster = 5 if cat_cluster==99 & strpos(category_clean,"统计数据与政策公开")>0

* 4.5 法律/法治/维权/消费者权益 -> 市场监管/司法治理 (8)
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"法律")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"法治")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"法援")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"消费维权")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"消费者权益保护")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"消费者权益")>0

* 4.6 环境/生态/环保 -> 市场监管 / 环境治理 (8)
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"环境保护")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"环境与")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"环境治理")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"环境监测")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"环境管理")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"环境空气质量")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"生态环境")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"生态保护")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"生态文明")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"生态环保")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"自然环境与生态保护")>0
replace cat_cluster = 8 if cat_cluster==99 & strpos(category_clean,"自然保护与生态环境")>0

* 4.7 文明创建 / 文化活动 / 红色叙事 / 纪念日 -> 城市形象 & 历史文化 (2)
replace cat_cluster = 2 if cat_cluster==99 & strpos(category_clean,"文化形象与城市活动")>0
replace cat_cluster = 2 if cat_cluster==99 & strpos(category_clean,"文化活动")>0
replace cat_cluster = 2 if cat_cluster==99 & strpos(category_clean,"民族传统与文化活动")>0
replace cat_cluster = 2 if cat_cluster==99 & strpos(category_clean,"社会文化与庆节活动")>0
replace cat_cluster = 2 if cat_cluster==99 & strpos(category_clean,"纪念日与庆祝活动")>0
replace cat_cluster = 2 if cat_cluster==99 & strpos(category_clean,"红色叙事")>0
replace cat_cluster = 2 if cat_cluster==99 & strpos(category_clean,"文明创建")>0
replace cat_cluster = 2 if cat_cluster==99 & strpos(category_clean,"文明城市")>0
replace cat_cluster = 2 if cat_cluster==99 & strpos(category_clean,"文明旅游")>0
replace cat_cluster = 2 if cat_cluster==99 & strpos(category_clean,"文明与文化活动")>0
replace cat_cluster = 2 if cat_cluster==99 & strpos(category_clean,"考古与文化遗产")>0

* 4.8 时政新闻 / 舆情 / 热点信息 -> 宣传 & 时政信息 (4)
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"新闻")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"热点信息")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"热点新闻")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"即时新闻")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"舆情")>0
replace cat_cluster = 4 if cat_cluster==99 & strpos(category_clean,"时事")>0

* 4.9 环境/疫情类遗漏的公共安全信息 -> 应急管理 & 公共安全 (3)
replace cat_cluster = 3 if cat_cluster==99 & strpos(category_clean,"疫情管理")>0
replace cat_cluster = 3 if cat_cluster==99 & strpos(category_clean,"疫情通报")>0
replace cat_cluster = 3 if cat_cluster==99 & strpos(category_clean,"疫情相关政策")>0
replace cat_cluster = 3 if cat_cluster==99 & strpos(category_clean,"水利管理与灾害防控")>0
replace cat_cluster = 3 if cat_cluster==99 & strpos(category_clean,"气象与天气信息")>0

* 4.10 最终剩下的仍为 99
replace cat_cluster = 99 if missing(cat_cluster)

* 更新 cat_cluster_name
replace cat_cluster_name = "公共服务 / Public service"            if cat_cluster==1
replace cat_cluster_name = "城市形象 & 历史文化"                  if cat_cluster==2
replace cat_cluster_name = "应急管理 & 公共安全"                  if cat_cluster==3
replace cat_cluster_name = "意识形态 & 宣传教育"                  if cat_cluster==4
replace cat_cluster_name = "政策信息 & 政务公开"                  if cat_cluster==5
replace cat_cluster_name = "教育 / 医疗 / 养老 / 社会保障"        if cat_cluster==6
replace cat_cluster_name = "人事任免 & 招聘"                      if cat_cluster==7
replace cat_cluster_name = "市场监管 / 执法 / 社会治理"           if cat_cluster==8
replace cat_cluster_name = "农业 / 农村发展 / 乡村振兴"           if cat_cluster==9
replace cat_cluster_name = "国际事务 / 对外合作"                  if cat_cluster==10
replace cat_cluster_name = "其他 / 不适用"                        if cat_cluster==99

*--------------------------------------------------------------
* 5. 合并小类：生成更粗的大类 cat_merge
*--------------------------------------------------------------

capture drop cat_merge cat_merge_name
gen byte cat_merge = .

* 1 公共服务
replace cat_merge = 1 if cat_cluster == 1

* 2 城市形象与文化
replace cat_merge = 2 if cat_cluster == 2

* 3 应急管理与公共安全
replace cat_merge = 3 if cat_cluster == 3

* 4 意识形态与宣传教育（宣传 + 政策宣传）
replace cat_merge = 4 if inlist(cat_cluster, 4, 5)

* 5 民生事业（教育/医疗/养老/社保 + 农业/农村）
replace cat_merge = 5 if inlist(cat_cluster, 6, 9)

* 6 监管与治理（市场监管/执法 + 人事任免 + 国际事务）
replace cat_merge = 6 if inlist(cat_cluster, 8, 7, 10)

* 8 其他
replace cat_merge = 8 if cat_cluster == 99 | missing(cat_merge)

* 标签
label define cat_merge_lbl ///
    1 "公共服务" ///
    2 "城市形象与文化" ///
    3 "应急管理与公共安全" ///
    4 "意识形态与宣传教育（含政策信息）" ///
    5 "民生事业（教育/医疗/养老/社保/农业）" ///
    6 "监管与治理（执法/市场监管/人事/国际事务）" ///
    8 "其他"

label values cat_merge cat_merge_lbl

gen str40 cat_merge_name = ""
replace cat_merge_name = "Public services"                          if cat_merge==1
replace cat_merge_name = "City branding and culture"                if cat_merge==2
replace cat_merge_name = "Emergency management and public safety"   if cat_merge==3
replace cat_merge_name = "Propaganda and political information"     if cat_merge==4
replace cat_merge_name = "Social services and welfare" if cat_merge==5
replace cat_merge_name = "Regulation and governance"                if cat_merge==6
replace cat_merge_name = "Other"                                    if cat_merge==8


keep if read_num!=0

graph bar (mean) read_num, ///
    over(cat_merge_name, label(angle(90))) ///
    blabel(bar, format(%9.1f) color(black)) ///
    ytitle("Mean Read Num") ///
    bargap(20)


corr read_num like_num share_num look_num collect_num
sum read_num like_num share_num look_num collect_num

tabstat look_num, by(year) stat(mean)


tabstat read_num like_num share_num look_num collect_num, ///
    by(cat_merge_name) ///
    stat(mean sd min max n) ///
    format(%9.1f)









