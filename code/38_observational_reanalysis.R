# Reanalysis of archived observations. No engagement counts or dates are simulated.
library(data.table)
library(fixest)
library(sandwich)
args<-commandArgs(FALSE)
script<-normalizePath(sub('^--file=','',grep('^--file=',args,value=TRUE)[1]))
root<-dirname(dirname(script));out<-file.path(root,'analysis/output/observational')
for(p in c('data','tables','figures','verification'))dir.create(file.path(out,p),recursive=TRUE,showWarnings=FALSE)
d<-readRDS(file.path(root,'data/wechat_posts_clean.rds'))
raw<-fread(file.path(root,'data/articles.csv'),select=c('id','read_num','like_num','look_num','collect_num'),showProgress=FALSE)
stopifnot(!anyDuplicated(d$id),!anyDuplicated(raw$id))
idx<-match(d$id,raw$id);stopifnot(!anyNA(idx))
raw<-raw[idx]
missing<-vapply(raw[,!c('id')],function(x)sum(is.na(x)),integer(1))
for(v in c('read_num','like_num','look_num','collect_num'))set(d,j=v,value=raw[[v]])
rm(raw);gc()
d[,has_reads:=!is.na(read_num)&read_num>0]
d[,complete_counts:=has_reads & !is.na(like_num)&like_num>=0 & !is.na(look_num)&look_num>=0]
families<-c('hard_propaganda','soft_propaganda','public_service','state_governance')
labels<-c(hard_propaganda='Hard propaganda',soft_propaganda='Soft propaganda',public_service='Public service',state_governance='State governance')
d[,family:=factor(content_family,levels=families)]
d[,month:=as.Date(format(publish_date,'%Y-%m-01'))]
d[,ad:=paste(account_id,publish_date)]
pop<-d[,.(posts=.N,positive=sum(has_reads),complete=sum(complete_counts)),by=family]
monthly<-d[,.(posts=.N,positive=sum(has_reads),coverage=mean(has_reads)),by=.(month,family)]
p<-d[complete_counts==TRUE]
p[,`:=`(log_reads=log1p(read_num),like_rate=like_num/read_num,
 recommend_rate=look_num/read_num,total_rate=(like_num+look_num)/read_num)]
p[,regime:=fifelse(publish_date<as.Date('2018-12-21'),'Before',fifelse(publish_date<as.Date('2020-07-01'),'Middle','After'))]
descriptive<-p[,.(N=.N,median_reads=median(read_num),mean_reads=mean(read_num),
 like=mean(like_rate),recommend=mean(recommend_rate),total=mean(total_rate)),by=.(regime,family)]

fmt<-function(x,digits=3)sprintf(paste0('%.',digits,'f'),x)
int<-function(x)format(x,big.mark=',',scientific=FALSE,trim=TRUE)
stars<-function(p)ifelse(p<0.01,'**',ifelse(p<0.05,'*',''))
cell<-function(b,p,scale=1)paste0(fmt(scale*b),ifelse(p<0.05,paste0('$^{',stars(p),'}$'),''))
table_write<-function(file,caption,label,headers,rows,note){
 lines<-c('\\begin{table}[H]','\\centering','\\setstretch{1}\\footnotesize',
 '\\setlength{\\tabcolsep}{5pt}','\\setlength{\\abovecaptionskip}{0pt}',
 paste0('\\caption{',caption,'}\\label{',label,'}'),'\\begin{threeparttable}',
 paste0('\\begin{tabular}{l',paste(rep('c',length(headers)-1),collapse=''),'}'),
 '\\toprule',paste0(paste(headers,collapse=' & '),' \\\\'),'\\midrule',
 vapply(rows,function(r)paste0(paste(r,collapse=' & '),' \\\\'),character(1)),
 '\\bottomrule','\\end{tabular}',
 '\\renewcommand{\\TPTnoteSettings}{\\leftmargin0pt\\labelwidth0pt\\labelsep0pt\\topsep0pt\\partopsep0pt\\itemsep0pt\\parsep0pt}',
 '\\begin{tablenotes}\\footnotesize\\setstretch{1}\\justifying',
 paste0('\\item[] Notes: ',note),'\\end{tablenotes}','\\end{threeparttable}','\\end{table}')
 writeLines(lines,file.path(out,'tables',file))
}
table_write('archive_coverage.tex','The archive and the positive-read analytical subset','tab:obscoverage',
 c('Family','Archived posts','Positive reads','Coverage (\\%)'),
 lapply(c('public_service','soft_propaganda','hard_propaganda','state_governance'),function(f){a<-pop[family==f];c(labels[f],int(a$posts),int(a$positive),fmt(100*a$positive/a$posts,2))}),
 'Original archive, 2015--2024. Coverage is the fraction with a recorded positive read count. A zero or missing read count provides no usable engagement-rate denominator; the archive does not establish its collection status. Family labels are the existing canonical classifications.')
table_write('archive_descriptives.tex','Recorded engagement after the restoration of likes','tab:obsdesc',
 c('Family','Posts','Median reads','Like (\\%)','Recommend (\\%)','Total (\\%)'),
 lapply(c('public_service','soft_propaganda','hard_propaganda','state_governance'),function(f){a<-descriptive[regime=='After' & family==f];c(labels[f],int(a$N),int(a$median_reads),fmt(100*a$like),fmt(100*a$recommend),fmt(100*a$total))}),
 'Original positive-read records, July 2020--December 2024, with complete nonnegative like and recommendation counts. Rates are equal-post averages of each count divided by reads. The denominator records views, rather than unique persons.')

# Comparable same-account/date publishing environments, without assuming identical readers.
mixed<-p[,.(families=uniqueN(family)),by=ad][families>1,ad]
pm<-p[ad %in% mixed]
read_models<-list(
 feols(log_reads~i(family,ref='hard_propaganda'),pm,vcov=~account_id+publish_date),
 feols(log_reads~i(family,ref='hard_propaganda')|account_id+publish_date,pm,vcov=~account_id+publish_date),
 feols(log_reads~i(family,ref='hard_propaganda')|ad,pm,vcov=~account_id+publish_date))
model_rows<-function(models,terms,scale=1){
 do.call(c,lapply(terms,function(t){list(c(labels[sub('family::','',t)],vapply(models,function(m)cell(coef(m)[t],pvalue(m)[t],scale),character(1))),
 c('',vapply(models,function(m)paste0('(',fmt(scale*se(m)[t]),')'),character(1))))}))
}
terms<-paste0('family::',c('public_service','soft_propaganda','state_governance'))
table_write('attention_regressions.tex','Content and readership in comparable publishing environments','tab:obsattention',
 c('','(1)','(2)','(3)'),c(list(c('Outcome',rep('$\\log(1+\\mathrm{reads})$',3))),model_rows(read_models,terms),
 list(c('Controls',rep('No',3)),c('Account fixed effects','No','Yes','No'),c('Date fixed effects','No','Yes','No'),
 c('Account--date fixed effects','No','No','Yes'),c('$N$',vapply(read_models,function(m)int(nobs(m)),character(1))),
 c('$R^2$',vapply(read_models,function(m)fmt(fitstat(m,'r2')$r2),character(1))))),
 'Original positive-read records on account--dates containing multiple families. Hard propaganda is the reference family. Parentheses contain standard errors clustered by account and date. Same account--date establishes a common publishing environment, with reader selection specific to each post. * $p<0.05$, ** $p<0.01$.')
post<-p[publish_date>=as.Date('2020-07-01')]
postmix<-post[ad %in% post[,.(families=uniqueN(family)),by=ad][families>1,ad]]
rate_models<-lapply(c('like_rate','recommend_rate','total_rate'),function(y)feols(as.formula(paste(y,'~i(family,ref="hard_propaganda")|ad')),postmix,vcov=~account_id+publish_date))
table_write('engagement_regressions.tex','Content and reactions within account--dates','tab:obsreactions',
 c('','(1)','(2)','(3)'),c(list(c('Outcome (pp)','Like','Recommendation','Total')),model_rows(rate_models,terms,100),
 list(c('Controls',rep('No',3)),c('Account--date fixed effects',rep('Yes',3)),
 c('$N$',vapply(rate_models,function(m)int(nobs(m)),character(1))),c('$R^2$',vapply(rate_models,function(m)fmt(fitstat(m,'r2')$r2),character(1))))),
 'Original positive-read records, July 2020--December 2024, on account--dates containing multiple families. Hard propaganda is the reference. Parentheses contain account- and date-clustered standard errors. These are conditional content associations. * $p<0.05$, ** $p<0.01$.')

# Daily content mix uses all archived posts; propaganda readership uses available counts.
mix<-d[,.(n_posts=.N,service_share=mean(family=='public_service'),governance_share=mean(family=='state_governance'),
 n_service=sum(family=='public_service'),n_prop=sum(family %in% c('hard_propaganda','soft_propaganda'))),by=.(account_id,publish_date)]
pr<-p[family %in% c('hard_propaganda','soft_propaganda'),.(mean_prop=mean(read_num),median_prop=median(read_num),n_measured=.N),by=.(account_id,publish_date)]
crowd<-merge(mix[n_service>0 & n_prop>0],pr,by=c('account_id','publish_date'))
crowd[,`:=`(log_mean=log1p(mean_prop),log_median=log1p(median_prop),log_posts=log(n_posts))]
crowd_models<-list(feols(log_mean~service_share+governance_share+log_posts|account_id+publish_date,crowd,vcov=~account_id+publish_date),
 feols(log_median~service_share+governance_share+log_posts|account_id+publish_date,crowd,vcov=~account_id+publish_date))
table_write('attention_allocation.tex','Content mix and available propaganda readership','tab:obsallocation',
 c('','(1)','(2)'),c(list(c('Outcome','Log mean reads','Log median reads')),
 do.call(c,lapply(c('service_share','governance_share','log_posts'),function(t)list(
 c(switch(t,service_share='Service-post share',governance_share='Governance-post share',log_posts='Log total posts'),vapply(crowd_models,function(m)cell(coef(m)[t],pvalue(m)[t]),character(1))),
 c('',vapply(crowd_models,function(m)paste0('(',fmt(se(m)[t]),')'),character(1)))))),
 list(c('Controls',rep('Yes',2)),c('Account fixed effects',rep('Yes',2)),c('Date fixed effects',rep('Yes',2)),
 c('$N$',vapply(crowd_models,function(m)int(nobs(m)),character(1))),c('$R^2$',vapply(crowd_models,function(m)fmt(fitstat(m,'r2')$r2),character(1))))),
 'Account--dates with service and propaganda posts in the full archive and at least one propaganda post with positive reads. Content shares use all archived posts; outcomes use available propaganda read counts. Parentheses contain account- and date-clustered standard errors. Content mix is chosen by publishers. * $p<0.05$, ** $p<0.01$.')

# Temporal estimators use daily units; fitted HAC intervals are approximate.
daily<-p[,.(like_rate=mean(like_rate),recommend_rate=mean(recommend_rate),total_rate=mean(total_rate),
 log_reads=mean(log_reads),n_available=.N),by=publish_date]
counts<-d[,.(n_posts=.N,coverage=mean(has_reads),service_share=mean(family=='public_service')),by=publish_date]
daily<-merge(daily,counts,by='publish_date');setorder(daily,publish_date)
local_fit<-function(x,y,h=30,lag=7,donut=0){
 keep<-abs(x)<=h & abs(x)>=donut & is.finite(y);x<-x[keep];y<-y[keep];ord<-order(x);x<-x[ord];y<-y[ord]
 post<-as.integer(x>=0);w<-1-abs(x)/(h+1);m<-lm(y~post+x+post:x,weights=w)
 V<-NeweyWest(m,lag=lag,prewhite=FALSE,adjust=TRUE)
 b<-coef(m)['post'];e<-sqrt(V['post','post'])
 list(model=m,V=V,b=unname(b),error=e,p=2*pnorm(-abs(b/e)),N=nobs(m),r2=summary(m)$r.squared,x=x,y=y)
}
series<-function(cut){z<-copy(daily);z[,x:=as.integer(publish_date-as.Date(cut))];z[abs(x)<=60]}
year_pair<-function(y,o){a<-series(paste0(y,'-07-01'));b<-series(paste0(y-1,'-07-01'))
 z<-merge(a[,.(x,y=get(o))],b[,.(x,y=get(o))],by='x',suffixes=c('_event','_comparison'))
 z[,delta:=y_event-y_comparison];z}
s18<-series('2018-12-21');s19<-series('2019-07-01');s20<-series('2020-07-01')
timefits<-list(local_fit(s18$x,s18$total_rate),local_fit(s20$x,s20$total_rate),
 local_fit(year_pair(2020,'total_rate')$x,year_pair(2020,'total_rate')$delta),
 local_fit(year_pair(2020,'like_rate')$x,year_pair(2020,'like_rate')$delta),
 local_fit(year_pair(2020,'recommend_rate')$x,year_pair(2020,'recommend_rate')$delta))
table_write('temporal_regressions.tex','Temporal changes in the recorded reaction subset','tab:obstime',
 c('','(1)','(2)','(3)','(4)','(5)'),list(c('Outcome (pp)','Total','Total','Total','Like','Recommend'),
 c('Event','Dec. 2018','July 2020','July 2020','July 2020','July 2020'),
 c('Year difference','No','No','Yes','Yes','Yes'),
 c('Post-cutoff',vapply(timefits,function(f)cell(f$b,f$p,100),character(1))),
 c('',vapply(timefits,function(f)paste0('(',fmt(100*f$error),')'),character(1))),
 c('Additional controls',rep('No',5)),c('Local linear trends',rep('Yes',5)),c('$N$',vapply(timefits,`[[`,numeric(1),'N')),
 c('$R^2$',vapply(timefits,function(f)fmt(f$r2),character(1)))),
 'Original positive-read records aggregated to daily equal-post rates. All fits use a 30-day window, triangular weights, and separate linear trends. Columns (3)--(5) fit paired 2020 minus 2019 daily rates. Parentheses contain fitted lag-7 Newey--West standard errors; intervals are approximate in a short common-date series. * $p<0.05$, ** $p<0.01$.')
temporal_sensitivity<-rbindlist(lapply(c(14,30,45),function(h)rbindlist(lapply(c(3,7,14),function(lag){z<-year_pair(2020,'total_rate');f<-local_fit(z$x,z$delta,h,lag);data.table(h=h,lag=lag,b=f$b,error=f$error,N=f$N,r2=f$r2)}))))
fwrite(temporal_sensitivity,file.path(out,'data','temporal_sensitivity.csv'))
source(file.path(root,'code/40_observational_presentation.R'))
diagnostic<-rbindlist(lapply(c('2018-12-21','2019-07-01','2020-06-29','2020-07-01'),function(cut){z<-series(cut);rbindlist(lapply(c('coverage','service_share','log_reads'),function(o){f<-local_fit(z$x,z[[o]]);data.table(cut=cut,outcome=o,b=f$b,error=f$error,p=f$p,N=f$N,r2=f$r2)}))}))
fwrite(diagnostic,file.path(out,'data','temporal_measurement_checks.csv'))
table_write('measurement_checks.tex','Changes in coverage, publishing mix, and readership','tab:obsmeasurement',
 c('','(1)','(2)','(3)'),
 c(do.call(c,lapply(c('coverage','service_share','log_reads'),function(o){z<-diagnostic[outcome==o & cut %in% c('2018-12-21','2020-06-29','2020-07-01')][match(c('2018-12-21','2020-06-29','2020-07-01'),cut)];
 list(c(switch(o,coverage='Positive-read coverage (pp)',service_share='Service-post share (pp)',log_reads='Mean log reads'),
 vapply(seq_len(3),function(i)cell(z$b[i],z$p[i],if(o=='log_reads')1 else 100),character(1))),
 c('',vapply(seq_len(3),function(i)paste0('(',fmt(z$error[i]*if(o=='log_reads')1 else 100),')'),character(1))),
 c('$R^2$',fmt(z$r2)))})),list(c('Additional controls',rep('No',3)),c('Local linear trends',rep('Yes',3)),c('$N$',rep('61',3)))),
 'Columns (1)--(3) use December 21, 2018, June 29, 2020, and July 1, 2020, respectively. Each outcome panel is a separate daily local linear model with a 30-day window and lag-7 Newey--West standard errors. Each model has 61 daily observations. Coverage and service shares use the complete archive; log reads use positive-read posts. * $p<0.05$, ** $p<0.01$.')

# Alternative dates, transition exclusions, and capped-read sensitivity.
pair_at<-function(cut,o,cap=FALSE){
 source<-if(cap)p[read_num<100000,.(rate=mean(get(o))),by=publish_date] else daily[,.(publish_date,rate=get(o))]
 other<-as.Date(cut);previous<-as.Date(paste0(as.integer(format(other,'%Y'))-1,format(other,'-%m-%d')))
 a<-copy(source);a[,x:=as.integer(publish_date-other)];b<-copy(source);b[,x:=as.integer(publish_date-previous)]
 z<-merge(a[abs(x)<=45,.(x,event=rate)],b[abs(x)<=45,.(x,comparison=rate)],by='x');z[,delta:=event-comparison];z
}
alternatives<-list(
 list(cut='2020-07-01',donut=0,cap=FALSE),list(cut='2020-06-29',donut=0,cap=FALSE),
 list(cut='2020-07-01',donut=3,cap=FALSE),list(cut='2020-07-01',donut=7,cap=FALSE),
 list(cut='2020-07-01',donut=0,cap=TRUE))
afits<-lapply(alternatives,function(a){z<-pair_at(a$cut,'total_rate',a$cap);local_fit(z$x,z$delta,donut=a$donut)})
table_write('date_and_record_sensitivity.tex','Alternative dates, transition windows, and read counters','tab:obsdates',
 c('','(1)','(2)','(3)','(4)','(5)'),
 list(c('Cutoff','July 1','June 29','July 1','July 1','July 1'),c('Exclude $|x|<$','0','0','3','7','0'),
 c('Reads $<100{,}000$','No','No','No','No','Yes'),
 c('Post-cutoff (pp)',vapply(afits,function(f)cell(f$b,f$p,100),character(1))),
 c('',vapply(afits,function(f)paste0('(',fmt(100*f$error),')'),character(1))),
 c('Additional controls',rep('No',5)),c('Local linear trends',rep('Yes',5)),
 c('$N$',vapply(afits,`[[`,numeric(1),'N')),c('$R^2$',vapply(afits,function(f)fmt(f$r2),character(1)))),
 'Paired 2020 minus 2019 daily total rates. All fits use a 30-day window and triangular weights. Parentheses contain lag-7 Newey--West standard errors. Transition exclusions remove days on both sides. Excluding high read counters changes the analytical population. * $p<0.05$, ** $p<0.01$.')

table_write('proposed_design.tex','Proposed randomized tests of audience and menu effects','tab:obsproposal',
 c('Module','Randomized factor','Primary comparison'),
 list(c('Audience','Peer visibility $P$','Named versus count-only reaction'),
 c('Audience','Operator visibility $O$','Identified versus aggregate records'),
 c('Menu','Available buttons','Named-only versus dual menu'),
 c('Mechanism extension','Verified norm information','Information versus control')),
 'Design proposals. Audience conditions form a $2\\times2$ factorial design, with identical labels, posts, and distribution consequences. The menu module uses an independent sample. No human-subject outcomes or registration are claimed.')

pdf_open<-function(file,w=8,h=4.8){pdf(file.path(out,'figures',file),width=w,height=h,family='Times',useDingbats=FALSE);par(las=1,bty='l',mgp=c(2.8,.8,0))}
colors<-c(public_service='blue',soft_propaganda='red',hard_propaganda='black',state_governance='grey35')
pdf_open('archive_coverage.pdf');par(mar=c(4.2,4.5,.7,.7))
plot(range(monthly$month),range(100*monthly$coverage),type='n',xlab='Publication month',ylab='Posts with positive recorded reads (%)')
for(f in c('public_service','soft_propaganda','hard_propaganda')){z<-monthly[family==f];setorder(z,month);lines(z$month,100*z$coverage,col=colors[f],lty=match(f,c('public_service','soft_propaganda','hard_propaganda')),lwd=1.3)}
legend('topleft',unname(labels[c('public_service','soft_propaganda','hard_propaganda')]),col=colors[c('public_service','soft_propaganda','hard_propaganda')],lty=1:3,bty='n',horiz=TRUE,cex=.86);dev.off()

pdf_open('observed_family_actions.pdf',8.5,4.6);par(mfrow=c(1,3),mar=c(4.7,4.1,1.4,.5))
ord<-c('public_service','soft_propaganda','hard_propaganda')
for(k in 1:3){y<-c('median_reads','like','recommend')[k];z<-descriptive[regime=='After'][match(ord,family)]
 values<-z[[y]]*if(k==1)1 else 100
 plot(1:3,values,pch=c(16,17,15),col=colors[ord],xaxt='n',xlab='',ylab=c('Median recorded reads','Mean likes / reads (%)','Mean recommendations / reads (%)')[k],
      xlim=c(.7,3.3),ylim=c(0,max(values)*1.2),main=c('A. Reading','B. Like','C. Recommendation')[k]);axis(1,1:3,c('Service','Soft','Hard'))}
dev.off()

pdf_open('observed_regime_trends.pdf',8,4.8);par(mar=c(4.2,4.5,.7,.7))
mt<-p[,.(rate=mean(total_rate)),by=.(month,family)]
plot(range(mt$month),range(100*mt$rate),type='n',xlab='Publication month',ylab='Mean likes plus recommendations / reads (%)')
for(f in ord){z<-mt[family==f];setorder(z,month);lines(z$month,100*z$rate,col=colors[f],lty=match(f,ord))}
abline(v=as.numeric(as.Date(c('2018-12-21','2020-07-01'))),lty=3)
legend('topright',unname(labels[ord]),col=colors[ord],lty=1:3,bty='n',cex=.85);dev.off()

# Actual weekly contrasts, not a simulated or imposed pre/post path.
event<-list();pretest<-list()
for(o in c('total_rate','like_rate','recommend_rate')){
 z<-year_pair(2020,o);z<-z[x>=-42 & x<=41];z[,week:=factor(floor(x/7),levels=-6:5)]
 m<-lm(delta~relevel(week,ref='-1'),data=z);V<-NeweyWest(m,lag=7,prewhite=FALSE,adjust=TRUE)
 b<-c(coef(m)[-1],0);names(b)<-c(setdiff(-6:5,-1),-1)
 e<-c(sqrt(diag(V))[-1],0);names(e)<-names(b)
 event[[o]]<-data.table(week=-6:5,b=b[as.character(-6:5)],error=e[as.character(-6:5)])
 lead<-grep('week.*-[2-6]$',names(coef(m)));bb<-coef(m)[lead];VV<-V[lead,lead,drop=FALSE]
 pretest[[o]]<-c(wald=drop(t(bb)%*%solve(VV,bb)),df=length(bb),p=pchisq(drop(t(bb)%*%solve(VV,bb)),length(bb),lower.tail=FALSE))
}
pdf_open('observed_event_time.pdf',8.5,4.5);par(mfrow=c(1,3),mar=c(4.1,4.2,1.5,.4))
for(k in 1:3){z<-event[[k]];plot(z$week,100*z$b,pch=16,xlab='Week from July 1',ylab='Year contrast relative to week -1 (pp)',
 ylim=range(100*(z$b-1.96*z$error),100*(z$b+1.96*z$error)),main=c('A. Total','B. Like','C. Recommendation')[k]);abline(h=0,lty=2);abline(v=-.5,lty=3)
 a<-z$error>0;arrows(z$week[a],100*(z$b[a]-1.96*z$error[a]),z$week[a],100*(z$b[a]+1.96*z$error[a]),code=3,angle=90,length=.035)}
dev.off()
table_write('event_time_coefficients.tex','Weekly contrasts in the recorded reaction subset','tab:obsevent',
 c('Week','Total (pp)','Like (pp)','Recommend (pp)'),
 do.call(c,lapply(seq_len(12),function(i)list(c(event[[1]]$week[i],vapply(event,function(z)fmt(100*z$b[i]),character(1))),
 c('',vapply(event,function(z)paste0('(',fmt(100*z$error[i]),')'),character(1)))))),
 'Weekly means of the paired 2020 minus 2019 daily rate relative to week -1. Parentheses contain fitted lag-7 Newey--West standard errors. Week -1 is zero by normalization. The figure reports pointwise approximate 95\\% intervals.')

saveRDS(list(attention=read_models,reactions=rate_models,allocation=crowd_models,temporal=timefits,event=event,pretest=pretest,
 diagnostics=diagnostic,descriptives=descriptive,population=pop,monthly=monthly),file.path(out,'data','observational_results.rds'))
fwrite(pop,file.path(out,'data','population_coverage.csv'));fwrite(descriptive,file.path(out,'data','observed_descriptives.csv'))
mac<-function(n,v,digits=3)paste0('\\newcommand{\\',n,'}{',fmt(v,digits),'}')
readb<-coef(read_models[[3]])['family::public_service']
macros<-c(mac('ObsCoverage',100*mean(d$has_reads),2),mac('ObsServiceRatio',exp(readb),2),mac('ObsServiceLog',readb),
 mac('ObsLikeService',100*descriptive[regime=='After' & family=='public_service',like]),
 mac('ObsLikeHard',100*descriptive[regime=='After' & family=='hard_propaganda',like]),
 mac('ObsRecService',100*descriptive[regime=='After' & family=='public_service',recommend]),
 mac('ObsRecHard',100*descriptive[regime=='After' & family=='hard_propaganda',recommend]),
 mac('ObsNaiveTwenty',100*timefits[[2]]$b),mac('ObsNetTwenty',100*timefits[[3]]$b),
 mac('ObsNetLike',100*timefits[[4]]$b),mac('ObsNetRec',100*timefits[[5]]$b),mac('ObsEighteen',100*timefits[[1]]$b),
 mac('ObsPreP',pretest$total_rate['p']))
writeLines(macros,file.path(out,'tables','observational_macros.tex'))
checks<-list(archive_posts=nrow(d),archive_accounts=uniqueN(d$account_id),first=min(d$publish_date),last=max(d$publish_date),
 positive_reads=sum(d$has_reads),complete_counts=nrow(p),raw_missing=missing,topcoded=sum(p$read_num>=100000),
 mixed_account_dates=length(mixed),within_account_date_N=nobs(read_models[[3]]),
 temporal_identity=timefits[[3]]$b-timefits[[4]]$b-timefits[[5]]$b,
 preperiod=pretest$total_rate,service_read_ratio=exp(readb),crowding_coefficient=coef(crowd_models[[1]])['service_share'])
stopifnot(abs(checks$temporal_identity)<1e-12,sum(pop$posts)==nrow(d),sum(pop$positive)==sum(d$has_reads))
writeLines(c('# Observational reanalysis verification','',capture.output(str(checks))),file.path(out,'verification/data_checks.md'))
print(checks);print(descriptive);print(vapply(timefits,`[[`,numeric(1),'b'))
source(file.path(root, 'code/40_observational_presentation.R'))
source(file.path(root, 'code/41_hypothesis_design_alignment.R'))
