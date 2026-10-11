# Full empirical teaching package: temporal redesign and two randomized modules.
# All observations and potential outcomes are synthetic; no human study is claimed.
args <- commandArgs(FALSE)
script <- normalizePath(sub('^--file=', '', grep('^--file=', args, value=TRUE)[1]))
root <- dirname(dirname(script))
source(file.path(root,'code','35_teaching_rebuild.R'))
out <- file.path(root,'analysis','output','teaching')
for(p in c('tables','figures','data','design','logs')) dir.create(file.path(out,p),recursive=TRUE,showWarnings=FALSE)
for(p in c('tables','figures')) file.copy(list.files(file.path(root,'teaching',p),full.names=TRUE),file.path(out,p),overwrite=TRUE)
write_table_original <- write_table
write_table <- function(file,caption,label,headers,rows,note) {
  write_table_original(file,caption,label,headers,rows,note)
  path <- file.path(out,'tables',file)
  txt <- readLines(path)
  txt <- sub('\\\\setstretch\\{1\\}', '\\\\setstretch{1}\\\\footnotesize',txt)
  txt <- sub('\\\\begin\\{tablenotes\\}', '\\\\begin{tablenotes}[flushleft]',txt)
  writeLines(txt,path)
}

colors <- c(service='#0072B2',soft='#009E73',hard='#D55E00')
pdf_open <- function(file,w=8,h=4.8) {
  pdf(file.path(out,'figures',file),width=w,height=h,family='Times',useDingbats=FALSE)
  par(las=1,cex=.96,bty='l',mgp=c(2.7,.75,0),tcl=-.25)
}

# General known-process covariance, including direct family contrasts.
daily_cov <- function(d,outcome,year) {
  d <- d[d$year==year,]
  q <- switch(outcome,like_rate=d$p_like,visible_rate=d$p_visible-d$shock,
              total_rate=d$p_like+d$p_visible-d$shock)
  av <- if(outcome=='like_rate')0 else .00010^2/(1-.6^2)
  n <- as.vector(table(factor(d$x,levels=-45:45)))
  vs <- as.vector(rowsum((q*(1-q)-av)/d$reads,d$x,reorder=TRUE))/n^2
  diag(vs)+av*.6^abs(outer(-45:45,-45:45,'-'))
}
known_fit <- function(d,o,h=30,method='difference') {
  f <- estimate(d,o,method=method,h=h)
  V <- daily_cov(d,o,2020)
  if(method=='difference')V <- V+daily_cov(d,o,2019)
  ii <- f$x+46
  f$known_standard_error <- sqrt(drop(t(f$linear_weights)%*%V[ii,ii]%*%f$linear_weights))
  f
}

# Readership distributions: one fixed generating population throughout the redesign.
pdf_open('content_readership.pdf',8,4.5)
par(mar=c(4.0,4.2,.7,.5))
boxplot(log10(reads)~family,data=main,names=unname(labels),col=adjustcolor(colors,.30),
        border=colors,outline=FALSE,ylab='Reader trials per post (log scale)',yaxt='n')
axis(2,at=log10(c(100,300,1000,3000,10000)),labels=c('100','300','1,000','3,000','10,000'))
dev.off()

# Nonparametric weekly event-time contrasts; reference is days -7 to -1.
event <- list()
pretest <- list()
for(o in c('total_rate','like_rate','visible_rate')) {
  z<-daily(main,o); y<-z$y[z$year==2020]-z$y[z$year==2019]
  V<-daily_cov(main,o,2019)+daily_cov(main,o,2020)
  weeks <- -6:5
  L <- t(vapply(weeks,function(k){a<-numeric(91);a[which(floor((-45:45)/7)==k)]<-1/7;
    a[which(floor((-45:45)/7)==-1)]<-a[which(floor((-45:45)/7)==-1)]-1/7;a},numeric(91)))
  b<-drop(L%*%y); S<-L%*%V%*%t(L); e<-sqrt(pmax(0,diag(S)))
  event[[o]]<-data.frame(week=weeks,b=b,error=e)
  i<-which(weeks < -1)
  if(all(diag(S[i,i])==0))pretest[[o]]<-c(statistic=0,df=0,p=NA_real_) else {
    wald<-drop(t(b[i])%*%solve(S[i,i],b[i]))
    pretest[[o]]<-c(statistic=wald,df=length(i),p=pchisq(wald,length(i),lower.tail=FALSE))
  }
}
pdf_open('event_time.pdf',8.5,4.2)
par(mfrow=c(1,3),mar=c(4.1,3.8,1.5,.4))
for(o in names(event)) {
  z<-event[[o]]; plot(z$week,100*z$b,pch=19,ylim=range(100*(z$b+1.96*z$error),100*(z$b-1.96*z$error)),
  xlab='Week from cutoff',ylab='Change from week -1 (pp)',
  main=switch(o,total_rate='A. Total',like_rate='B. Like',visible_rate='C. Recommendation'))
  abline(h=0,col='grey55',lty=2);abline(v=-.5,lty=3,col='grey55')
  nz<-z$error>0
  arrows(z$week[nz],100*(z$b[nz]-1.96*z$error[nz]),z$week[nz],100*(z$b[nz]+1.96*z$error[nz]),angle=90,code=3,length=.035)
  points(z$week,100*z$b,pch=19)
}
dev.off()

family_fits <- setNames(lapply(families,function(f)setNames(lapply(c('like_rate','visible_rate','total_rate'),
  function(o)known_fit(main[main$family==f,],o)),c('like_rate','visible_rate','total_rate'))),families)
pdf_open('family_effects.pdf',8,4.8)
par(mar=c(4.2,8.1,.6,.8))
locations <- c(8,5,2); offsets<-c(.55,0,-.55)
vals<-unlist(lapply(family_fits,function(ff)vapply(ff,function(a)100*a$tau,numeric(1))))
errs<-unlist(lapply(family_fits,function(ff)vapply(ff,function(a)100*a$known_standard_error,numeric(1))))
plot(NA,xlim=range(c(vals-1.96*errs,vals+1.96*errs,0)),ylim=c(.7,9),xlab='Redesign effect (pp)',ylab='',yaxt='n')
axis(2,at=locations,labels=unname(labels),tick=FALSE)
abline(v=0,col='grey60',lty=2)
for(j in seq_along(families))for(k in 1:3) {
  i<-(j-1)*3+k; yy<-locations[j]+offsets[k]
  segments(vals[i]-1.96*errs[i],yy,vals[i]+1.96*errs[i],yy,col=colors[j],lwd=1.5)
  points(vals[i],yy,pch=c(16,17,15)[k],col=colors[j],cex=1.1)
}
legend('bottomright',c('Like','Recommendation','Total'),pch=c(16,17,15),bty='n',horiz=TRUE,cex=.8)
dev.off()
write_table('family_regressions.tex','Content heterogeneity in simulated redesign effects','tab:familyreg',
 c('Outcome (pp)',unname(labels)),do.call(c,lapply(c('like_rate','visible_rate','total_rate'),function(o){
  fs<-lapply(family_fits,`[[`,o);list(c(switch(o,like_rate='Like',visible_rate='Recommendation',total_rate='Total'),coef_cells(fs)),c('',error_cells(fs)))})),
 'Fully synthetic data. Each estimate fits the same 30-day difference in discontinuities to a family-specific daily rate. Parentheses contain known-process standard errors. Each family has 60 posts per day, and each fit uses 61 date pairs.')

# Complete factorial assignment at the person level; item pool has 3 items per family.
set.seed(20261011L)
N <- 2400L
people <- data.frame(id=1:N,u=rnorm(N),peer=rep(c(0,0,1,1),each=N/4),
                     operator=rep(c(0,1,0,1),each=N/4))
people[,c('peer','operator')] <- people[sample.int(N),c('peer','operator')]
expdat <- people[rep(1:N,each=3),]
expdat$family <- factor(rep(families,N),levels=families)
expdat$item <- factor(paste(expdat$family,sample(1:3,nrow(expdat),replace=TRUE),sep='_'),
                      levels=as.vector(outer(families,1:3,paste,sep='_')))
item_effect <- setNames(c(-.18,.05,.13,.11,-.14,.03,.07,.08,-.15),levels(expdat$item))
expdat$norm <- c(service=.50,soft=-.15,hard=-.65)[expdat$family]+.55*expdat$u+rnorm(nrow(expdat),0,.55)
base <- 4.8+c(service=.0,soft=.15,hard=.25)[expdat$family]+.65*expdat$u+
        .20*expdat$norm+item_effect[expdat$item]+rnorm(nrow(expdat),0,.8)
peer_effect <- .75*expdat$norm
operator_effect <- c(service=.15,soft=.20,hard=.25)[expdat$family]
potential <- function(p,o)pmin(10,pmax(0,base+p*peer_effect+o*operator_effect+.10*p*o))
expdat$willingness <- potential(expdat$peer,expdat$operator)
expdat$probability <- plogis((expdat$willingness-5)/1.6)
expdat$click <- rbinom(nrow(expdat),1,expdat$probability)
expdat$soft <- as.integer(expdat$family=='soft');expdat$hard<-as.integer(expdat$family=='hard')

# Respondent-clustered OLS covariance with CR1 finite-sample adjustment.
cluster_fit <- function(formula,d) {
  m<-lm(formula,d);X<-model.matrix(m);bread<-solve(crossprod(X))
  score<-rowsum(X*residuals(m),d$id,reorder=FALSE);G<-nrow(score);n<-nrow(X);k<-ncol(X)
  V<-bread%*%crossprod(score)%*%bread*G/(G-1)*(n-1)/(n-k)
  list(model=m,V=V,G=G,df=G-1)
}
contrast <- function(f,L) {
  b<-coef(f$model);L<-L[names(b)];est<-sum(L*b);err<-sqrt(drop(t(L)%*%f$V%*%L))
  c(estimate=est,error=err,lower=est-qt(.975,f$df)*err,upper=est+qt(.975,f$df)*err,
    p=2*pt(-abs(est/err),f$df))
}
newmatrix<-function(f,d)model.matrix(delete.response(terms(f$model)),d,
  contrasts.arg=f$model$contrasts,xlev=f$model$xlevels)
audience_fit<-cluster_fit(willingness~item+peer+operator+peer:operator+
 peer:soft+peer:hard+operator:soft+operator:hard+peer:operator:soft+peer:operator:hard,expdat)
effect_contrast <- function(f,variable,family=NULL,norm=NULL) {
  a<-expdat[!duplicated(expdat$item),];if(!is.null(family))a<-a[a$family==family,]
  a$peer<-.5;a$operator<-.5;if(!is.null(norm))a$norm<-norm
  b<-a;a[[variable]]<-0;b[[variable]]<-1
  contrast(f,colMeans(newmatrix(f,b)-newmatrix(f,a)))
}
audience_effects<-lapply(c('peer','operator'),function(v)
  rbind(All=effect_contrast(audience_fit,v),do.call(rbind,lapply(families,function(f)effect_contrast(audience_fit,v,f)))))
names(audience_effects)<-c('peer','operator')
truth_peer<-mean((potential(1,0)-potential(0,0)+potential(1,1)-potential(0,1))/2)
truth_operator<-mean((potential(0,1)-potential(0,0)+potential(1,1)-potential(1,0))/2)
norm_fit<-cluster_fit(willingness~item+norm+norm:soft+norm:hard+peer+peer:norm+
 peer:soft+peer:hard+peer:norm:soft+peer:norm:hard+operator+operator:soft+operator:hard+
 peer:operator+peer:operator:soft+peer:operator:hard,expdat)
norm_effects<-t(vapply(c(-1,0,1),function(v)effect_contrast(norm_fit,'peer',norm=v),numeric(5)))
stars<-function(p)ifelse(p<.01,'***',ifelse(p<.05,'**',ifelse(p<.1,'*','')))
fcell<-function(z,scale=1)paste0(fmt(scale*z['estimate']),if(z['p']<.1)paste0('$^{',stars(z['p']),'}$')else'')
ecell<-function(z,scale=1)paste0('(',fmt(scale*z['error']),')')
write_table('audience_experiment.tex','Audience effects in the simulated randomized experiment','tab:experiment',
 c('','All items','Service','Soft','Hard'),list(
 c('Peer identification',apply(audience_effects$peer,1,fcell)),
 c('',apply(audience_effects$peer,1,ecell)),
 c('Operator identification',apply(audience_effects$operator,1,fcell)),
 c('',apply(audience_effects$operator,1,ecell)),
 c('Item fixed effects',rep('Yes',4)),c('Respondents',rep('2,400',4)),c('Responses','7,200','2,400','2,400','2,400'),
 c('Model $R^2$',rep(fmt(summary(audience_fit$model)$r.squared),4))),
 'Fully synthetic respondents and responses. Estimates are marginal contrasts from one OLS model with item fixed effects and audience interactions by family, averaged over the other randomized audience factor. The outcome is willingness on a 0--10 scale. Parentheses contain respondent-clustered standard errors. The all-item contrast gives equal weight to the three families. * $p<.1$, ** $p<.05$, *** $p<.01$.')

# Norms are measured before assignment; conditional effects do not establish mediation.
pdf_open('audience_effects.pdf',8,4.8)
par(mar=c(4.1,7.9,.7,.5))
yy<-c(8,6,4,2); rr<-rbind(audience_effects$peer,audience_effects$operator)
plot(NA,xlim=range(rr[,c('lower','upper')],0),ylim=c(1,9),yaxt='n',ylab='',xlab='Effect on willingness (0-10 scale)')
axis(2,at=yy,labels=c('All items',unname(labels)),tick=FALSE);abline(v=0,lty=2,col='grey55')
for(j in 1:4)for(k in 1:2){z<-audience_effects[[k]][j,];y<-yy[j]+c(.23,-.23)[k]
 segments(z['lower'],y,z['upper'],y,col=c('#0072B2','#D55E00')[k],lwd=1.5)
 points(z['estimate'],y,col=c('#0072B2','#D55E00')[k],pch=c(16,17)[k])}
legend('bottomright',c('Peer identification','Operator identification'),col=c('#0072B2','#D55E00'),pch=c(16,17),bty='n',cex=.85)
dev.off()
pdf_open('norm_moderation.pdf',7.5,4.5)
par(mar=c(4.1,4.6,.7,.6))
norm_grid<-seq(-1.5,1.5,length.out=61)
nr<-t(vapply(norm_grid,function(v)effect_contrast(norm_fit,'peer',norm=v),numeric(5)))
plot(norm_grid,nr[,'estimate'],type='n',ylim=range(nr[,c('lower','upper')]),xlab='Expected peer approval before assignment (standardized units)',ylab='Peer identification effect on willingness')
polygon(c(norm_grid,rev(norm_grid)),c(nr[,'lower'],rev(nr[,'upper'])),col=adjustcolor('#0072B2',.18),border=NA)
abline(h=0,lty=2,col='grey55');lines(norm_grid,nr[,'estimate'],col='#0072B2',lwd=2)
dev.off()

# Independent randomized menu module; stable utilities for the existing action.
set.seed(20261012L)
menu<-expdat[,c('id','u','family','item')]
expanded<-sample(rep(0:1,each=N/2));menu$expanded<-expanded[menu$id]
gumbel<-function(n)-log(-log(runif(n)))
u_none<-gumbel(nrow(menu))
u_named<- -2.0+.35*menu$u+c(service=.2,soft=-.2,hard=-.6)[menu$family]+gumbel(nrow(menu))
u_new<- -2.0+.35*menu$u+c(service=.0,soft=.7,hard=1.2)[menu$family]+gumbel(nrow(menu))
old<-as.integer(u_named>u_none)
full<-max.col(cbind(u_none,u_named,u_new),ties.method='first')-1L
menu$total<-ifelse(menu$expanded,as.integer(full>0),old)
menu$named<-ifelse(menu$expanded,as.integer(full==1),old)
menu$count_only<-ifelse(menu$expanded,as.integer(full==2),0)
menu$soft<-as.integer(menu$family=='soft');menu$hard<-as.integer(menu$family=='hard')
menu_effects<-lapply(c('total','named','count_only'),function(o){
 fit<-cluster_fit(as.formula(paste(o,'~item+expanded+expanded:soft+expanded:hard')),menu)
 a<-menu[!duplicated(menu$item),];a$expanded<-0;b<-a;b$expanded<-1
 list(fit=fit,effect=contrast(fit,colMeans(newmatrix(fit,b)-newmatrix(fit,a))))})
names(menu_effects)<-c('total','named','count_only')
write_table('menu_experiment.tex','Menu effects in the simulated randomized experiment','tab:menu',
 c('','Any reaction','Named action','Count-only action'),list(
 c('Expanded menu (pp)',vapply(menu_effects,function(z)fcell(z$effect,100),character(1))),
 c('',vapply(menu_effects,function(z)ecell(z$effect,100),character(1))),
 c('Item fixed effects',rep('Yes',3)),c('Respondents',rep('2,400',3)),c('Responses',rep('7,200',3)),
 c('$R^2$',vapply(menu_effects,function(z)fmt(summary(z$fit$model)$r.squared),character(1)))),
 'Fully synthetic data. A separate person-level assignment compares an existing named action alone with a menu adding a count-only action. Estimates average the randomized difference over the item pool. Parentheses contain respondent-clustered standard errors. Each participant chooses at most one action per item. * $p<.1$, ** $p<.05$, *** $p<.01$.')

# Randomization checks use respondent-level covariates, not post-treatment outcomes.
groups<-with(people,paste(peer,operator,sep=':'))
balance<-aggregate(people$u,list(cell=groups),function(z)c(n=length(z),mean=mean(z),sd=sd(z)))
write_table('randomization_balance.tex','Randomization balance in the simulated audience module','tab:balance',
 c('Peer','Operator','Respondents','Mean baseline trait','Standard deviation'),
 lapply(seq_len(nrow(balance)),function(i)c(strsplit(balance$cell[i],':',fixed=TRUE)[[1]],balance$x[i,1],fmt(balance$x[i,2]),fmt(balance$x[i,3]))),
 'Fully synthetic data. Complete factorial assignment fixes 600 respondents in each cell. The baseline trait is generated before assignment and is included here as a randomization check, without screening the realization for balance.')

saveRDS(expdat,file.path(out,'data','audience_experiment.rds'))
saveRDS(menu,file.path(out,'data','menu_experiment.rds'))
saveRDS(list(event=event,pretest=pretest,family=family_fits,audience=audience_fit,
 audience_effects=audience_effects,norm=norm_fit,norm_effects=norm_effects,menu=menu_effects,
 truth=c(peer=truth_peer,operator=truth_operator,menu_total=mean((full>0)-old),menu_named=mean((full==1)-old))),
 file.path(out,'data','causal_analysis_results.rds'))
mac<-readLines(file.path(out,'tables','result_macros.tex'))
addmacro<-function(name,v,digits=3)paste0('\\newcommand{\\',name,'}{',sprintf(paste0('%.',digits,'f'),v),'}')
mac<-c(mac,addmacro('SimNaive',100*fits[[1]]$tau),addmacro('PeerPooled',audience_effects$peer[1,'estimate']),
 addmacro('PeerService',audience_effects$peer[2,'estimate']),addmacro('PeerHard',audience_effects$peer[4,'estimate']),
 addmacro('OperatorPooled',audience_effects$operator[1,'estimate']),
 addmacro('NormNegative',norm_effects[1,'estimate']),addmacro('NormPositive',norm_effects[3,'estimate']),
 addmacro('MenuTotal',100*menu_effects$total$effect['estimate']),
 addmacro('MenuNamed',100*menu_effects$named$effect['estimate']),
 addmacro('MenuCount',100*menu_effects$count_only$effect['estimate']),
 addmacro('PreWaldP',pretest$total_rate['p']))
writeLines(mac,file.path(out,'tables','result_macros.tex'))
checks2<-list(audience_cell_counts=table(groups),audience_respondents=length(unique(expdat$id)),
 audience_responses=nrow(expdat),audience_model_rank=audience_fit$model$rank,
 full_rank=audience_fit$model$rank==length(coef(audience_fit$model)),
 menu_cell_counts=table(expanded),menu_identity=max(abs(menu$total-menu$named-menu$count_only)),
 menu_coefficient_identity=menu_effects$total$effect['estimate']-menu_effects$named$effect['estimate']-menu_effects$count_only$effect['estimate'],
 nested_menu_potential=all(as.integer(full>0)>=old),
 preperiod_wald=pretest$total_rate,truth_peer=truth_peer,truth_operator=truth_operator)
stopifnot(checks2$full_rank,checks2$menu_identity==0,abs(checks2$menu_coefficient_identity)<1e-10,
 checks2$nested_menu_potential,all(table(groups)==600),nrow(expdat)==7200)
writeLines(c('# Causal analysis verification','',capture.output(str(checks2))),file.path(out,'verification.md'))
for(path in list.files(file.path(out,'tables'),pattern='\\.tex$',full.names=TRUE)) {
 txt<-readLines(path)
 if(any(grepl('\\\\begin\\{table\\}',txt))){
  txt<-sub('^\\\\setstretch\\{1\\}$','\\\\setstretch{1}\\\\footnotesize',txt)
  txt<-sub('\\\\begin\\{tablenotes\\}\\\\footnotesize','\\\\begin{tablenotes}[flushleft]\\\\footnotesize',txt)
 }
 writeLines(txt,path)
}
cat('\nFull causal analysis completed.\n');print(checks2)
print(audience_effects);print(vapply(menu_effects,function(z)z$effect['estimate'],numeric(1)))
