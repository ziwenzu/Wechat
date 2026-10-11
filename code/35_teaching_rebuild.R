# Standalone teaching laboratory. Every row and outcome generated here is synthetic.
# Does not read, recode, move, or overwrite any original post or earlier simulation.
# Run from the project root: Rscript code/35_teaching_rebuild.R
args <- commandArgs(FALSE)
script <- normalizePath(sub('^--file=', '', grep('^--file=', args, value=TRUE)[1]))
root <- dirname(dirname(script))
out <- file.path(root, 'teaching')
for (p in c('data','tables','figures')) dir.create(file.path(out,p), recursive=TRUE, showWarnings=FALSE)

families <- c('service','soft','hard')
labels <- c(service='Public service', soft='Soft propaganda', hard='Hard propaganda')
seed <- 20261010L
panel <- expand.grid(account=1:60, family=families, x=-45:45, year=c(2019,2020),
                     KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
panel$family <- factor(panel$family, levels=families)
panel$post_id <- seq_len(nrow(panel))
panel$date <- as.Date(paste0(panel$year,'-07-01')) + panel$x
panel$post <- as.integer(panel$x >= 0)
set.seed(seed)
account_effect <- rnorm(60, 0, .35)
day_shocks <- lapply(1:2, function(z) as.numeric(arima.sim(list(ar=.6), n=91, sd=.00010)))
panel$shock <- unlist(day_shocks)[(panel$year-2019)*91 + panel$x + 46]
median_reads <- c(service=1700, soft=950, hard=550)
panel$reads <- pmax(50L, as.integer(round(exp(log(median_reads[panel$family]) +
   account_effect[panel$account] + rnorm(nrow(panel),0,.30)))))
panel$base_visible <- c(service=.006,soft=.007,hard=.009)[panel$family] +
                     .000006*panel$x + panel$shock

make_scenario <- function(scenario) {
  d <- panel
  redesign <- as.integer(d$year==2020 & d$post==1)
  new_like <- c(service=.006,soft=.010,hard=.014)[d$family]
  reduction <- c(service=.001,soft=.003,hard=.005)[d$family]
  d$p_like <- if (scenario=='calendar') rep(0,nrow(d)) else new_like*redesign
  d$p_visible <- d$base_visible
  if (scenario %in% c('audience','recommendation')) d$p_visible <- d$p_visible - reduction*redesign
  if (scenario=='calendar') d$p_visible <- d$p_visible + .004*d$post
  # The same random stream makes the two observationally equivalent mechanisms exact.
  set.seed(seed+1L)
  counts <- vapply(seq_len(nrow(d)), function(i)
    as.vector(rmultinom(1,d$reads[i],c(d$p_like[i],d$p_visible[i],1-d$p_like[i]-d$p_visible[i]))),
    numeric(3))
  d$likes <- as.integer(counts[1,]); d$visible <- as.integer(counts[2,])
  d$like_rate <- d$likes/d$reads
  d$visible_rate <- d$visible/d$reads
  d$total_rate <- d$like_rate+d$visible_rate
  d$scenario <- scenario
  d
}
scenarios <- setNames(lapply(c('audience','recommendation','calendar','expansion'),make_scenario),
                      c('audience','recommendation','calendar','expansion'))
saveRDS(scenarios, file.path(out,'data','synthetic_scenarios.rds'))

daily <- function(d, outcome, family=NULL) {
  if (!is.null(family)) d <- d[d$family==family,]
  z <- aggregate(d[[outcome]],list(year=d$year,x=d$x),mean)
  names(z)[3] <- 'y'
  z
}

# Local linear WLS on daily means, with Newey-West covariance of weighted scores.
# Bandwidth and lag are classroom design choices, fixed before fitting outcomes.
local_fit <- function(x,y,h=30L,lag=7L) {
  keep <- abs(x)<=h & is.finite(y)
  x<-x[keep]; y<-y[keep]; ord<-order(x); x<-x[ord]; y<-y[ord]
  post<-as.integer(x>=0); X<-cbind(1,post,x,post*x)
  w<-1-abs(x)/(h+1); Xw<-X*sqrt(w); yw<-y*sqrt(w)
  bread<-solve(crossprod(Xw)); b<-as.vector(bread%*%crossprod(Xw,yw))
  u<-as.vector(y-X%*%b); scores<-X*(w*u)
  meat<-crossprod(scores)
  for (l in seq_len(min(lag,nrow(X)-1))) {
    a<-crossprod(scores[(l+1):nrow(X),,drop=FALSE],scores[1:(nrow(X)-l),,drop=FALSE])
    meat<-meat+(1-l/(lag+1))*(a+t(a))
  }
  V<-bread%*%meat%*%bread*nrow(X)/(nrow(X)-ncol(X))
  mu<-sum(w*y)/sum(w)
  list(b=b,V=V,tau=b[2],standard_error=sqrt(V[2,2]),n=length(y),
       r2=1-sum(w*u^2)/sum(w*(y-mu)^2),
       x=x,linear_weights=as.vector((bread%*%t(X*w))[2,]))
}

# Known-process covariance is available only because the classroom DGP is public.
# It combines multinomial sampling variance with the exact stationary AR(1) covariance.
# This is a benchmark, not a proposal for assuming a known process in real platform data.
known_process_error <- function(d,outcome,method,f) {
  pl<-d$p_like; pv<-d$p_visible-d$shock
  probability<-switch(outcome,total_rate=pl+pv,like_rate=pl,visible_rate=pv)
  ar_variance<-if(outcome=='like_rate') 0 else .00010^2/(1-.6^2)
  index<-(d$year-2019)*91+d$x+46
  sampling_variance<-as.vector(rowsum((probability*(1-probability)-ar_variance)/d$reads,
                                     index,reorder=TRUE))/(180^2)
  selected<-f$x+46
  v<-sampling_variance[91+selected]
  multiplier<-1
  if(method=='difference'){v<-v+sampling_variance[selected];multiplier<-2}
  Sigma<-diag(v)+multiplier*ar_variance*.6^abs(outer(f$x,f$x,'-'))
  sqrt(as.numeric(t(f$linear_weights)%*%Sigma%*%f$linear_weights))
}
estimate <- function(d,outcome,method='difference',h=30L,lag=7L,family=NULL) {
  z<-daily(d,outcome,family)
  y20<-z[z$year==2020,]; y19<-z[z$year==2019,]
  y20<-y20[order(y20$x),]; y19<-y19[order(y19$x),]
  stopifnot(identical(y20$x,y19$x))
  y<-if(method=='difference') y20$y-y19$y else y20$y
  f<-local_fit(y20$x,y,h,lag)
  if(is.null(family))f$known_standard_error<-known_process_error(d,outcome,method,f)
  f
}

# Plain booktabs tables; standalone captions disclose simulation status.
write_table <- function(file,caption,label,headers,rows,note) {
  lines<-c('\\begin{table}[H]','\\centering','\\setstretch{1}',
    '\\setlength{\\tabcolsep}{4.5pt}','\\setlength{\\abovecaptionskip}{0pt}',
    paste0('\\caption{',caption,'}\\label{',label,'}'),
    '\\begin{threeparttable}',paste0('\\begin{tabular}{l',paste(rep('c',length(headers)-1),collapse=''),'}'),
    '\\toprule',paste0(paste(headers,collapse=' & '),' \\\\'),'\\midrule',
    vapply(rows,function(r) paste0(paste(r,collapse=' & '),' \\\\'),character(1)),
    '\\bottomrule','\\end{tabular}',
    '\\begin{tablenotes}\\footnotesize\\setstretch{1}\\justifying',
    paste0('\\item[] Notes: ',note),'\\end{tablenotes}','\\end{threeparttable}','\\end{table}')
  writeLines(lines,file.path(out,'tables',file))
}
fmt<-function(x) sprintf('%.3f',x)
coef_cells<-function(fits,scale=100) vapply(fits,function(f)fmt(scale*f$tau),character(1))
error_cells<-function(fits,scale=100) vapply(fits,function(f)paste0('(',fmt(scale*f$known_standard_error),')'),character(1))
main<-scenarios$audience
fits<-list(estimate(main,'total_rate','naive'),estimate(main,'total_rate'),
           estimate(main,'like_rate'),estimate(main,'visible_rate'))
write_table('main_estimates.tex','Redesign estimates in simulated teaching data','tab:main',
 c('','(1)','(2)','(3)','(4)'),list(
 c('Outcome','Total','Total','Like','Recommendation'),
 c('Contrast','2020 jump','Year difference','Year difference','Year difference'),
 c('Cutoff coefficient (pp)',coef_cells(fits)),c('',error_cells(fits)),
 c('Daily observations',vapply(fits,function(f)f$n,numeric(1))),
 c('$R^2$',vapply(fits,function(f)fmt(f$r2),character(1)))),
 'All values use fully simulated teaching data. Columns (2)--(4) use paired 2020 minus 2019 daily means; column (1) uses 2020 daily means. All columns use a 30-day window, triangular weights, and separate linear trends. Parentheses show known-process standard errors from the public simulation model, incorporating multinomial variance and AR(1) day shocks. A daily mean averages 180 synthetic posts.')

names_s<-c('Audience','Recommendation','Calendar','Expansion')
naive<-lapply(scenarios,estimate,outcome='total_rate',method='naive')
diff<-lapply(scenarios,estimate,outcome='total_rate')
write_table('scenario_estimates.tex','Alternative mechanisms in simulated teaching data','tab:scenarios',
 c('',names_s),list(c('2020 cutoff (pp)',coef_cells(naive)),c('',error_cells(naive)),
 c('Year-difference cutoff (pp)',coef_cells(diff)),c('',error_cells(diff)),
 c('Daily observations',rep(61,4))),
 'Fully simulated teaching data. Parentheses show known-process standard errors from the public simulation model. Each contrast uses a 30-day window. Audience and recommendation scenarios generate exactly the same article counters; their latent mechanisms differ. The calendar scenario contains no redesign effect. Expansion adds likes without changing the recommendation probability.')

postmain<-main[main$year==2020 & main$x>=0,]
rows<-lapply(families,function(f){z<-postmain[postmain$family==f,];
 c(labels[f],format(nrow(z),big.mark=','),sprintf('%.0f',median(z$reads)),
   fmt(100*mean(z$like_rate)),fmt(100*mean(z$visible_rate)),fmt(100*mean(z$total_rate)),
   fmt(sum(z$likes)/sum(z$likes+z$visible)))})
write_table('family_descriptives.tex','Content and engagement in simulated teaching data','tab:families',
 c('Family','Posts','Median reads','Like (\\%)','Recommend (\\%)','Total (\\%)','Like share'),rows,
 'Fully simulated teaching data, 2020 post-cutoff period. Rates average post-level action counts divided by simulated reader trials. Like share is a ratio of pooled counts, defined as likes divided by likes plus recommendations. Each reader chooses at most one action in this laboratory; real platform counters do not establish this restriction.')

sens<-lapply(c(14,30,45),function(h)estimate(main,'total_rate',h=h))
write_table('window_sensitivity.tex','Window sensitivity in simulated teaching data','tab:windows',
 c('','(1)','(2)','(3)'),list(c('Window (days)',14,30,45),
 c('Year-difference total (pp)',coef_cells(sens)),c('',error_cells(sens)),
 c('Daily observations',vapply(sens,function(f)f$n,numeric(1))),
 c('$R^2$',vapply(sens,function(f)fmt(f$r2),character(1)))),
 'Fully simulated teaching data. Identical outcome, local-linear estimator, and known-process covariance across windows.')

# Direct family contrast: daily (hard - service), then year difference, with joint covariance.
hard<-daily(main,'visible_rate','hard'); service<-daily(main,'visible_rate','service')
contrast<-merge(hard,service,by=c('year','x'),suffixes=c('_hard','_service'))
contrast$y<-contrast$y_hard-contrast$y_service
c20<-contrast[contrast$year==2020,];c19<-contrast[contrast$year==2019,]
c20<-c20[order(c20$x),];c19<-c19[order(c19$x),]
contrast_fit<-local_fit(c20$x,c20$y-c19$y)

# Repeated sampling checks coverage instead of treating one small standard error as validation.
set.seed(seed+1000L)
mc <- matrix(NA_real_,200,4,dimnames=list(NULL,c('estimate','standard_error','covers','known_covers')))
group <- (panel$year-2019)*91+panel$x+46
for (r in seq_len(nrow(mc))) {
  fresh <- unlist(lapply(1:2,function(z)as.numeric(arima.sim(list(ar=.6),n=91,sd=.00010))))
  pl <- main$p_like
  pv <- main$p_visible - panel$shock + fresh[group]
  nl <- rbinom(nrow(panel),panel$reads,pl)
  nz <- rbinom(nrow(panel),panel$reads-nl,pv/(1-pl))
  daily_rate <- as.vector(rowsum((nl+nz)/panel$reads,group,reorder=TRUE))/180
  f <- local_fit(-45:45,daily_rate[92:182]-daily_rate[1:91])
  mc[r,] <- c(f$tau,f$standard_error,abs(f$tau-.007)<=1.96*f$standard_error,
             abs(f$tau-.007)<=1.96*fits[[2]]$known_standard_error)
}
saveRDS(mc,file.path(out,'data','monte_carlo.rds'))
write_table('monte_carlo.tex','Repeated sampling in simulated teaching data','tab:mc',
 c('Statistic','Value'),list(c('Replications',nrow(mc)),
 c('True average effect (pp)',fmt(.7)),
 c('Mean estimate (pp)',fmt(100*mean(mc[,'estimate']))),
 c('Bias (pp)',fmt(100*(mean(mc[,'estimate'])-.007))),
 c('Root mean squared error (pp)',fmt(100*sqrt(mean((mc[,'estimate']-.007)^2)))),
 c('Lag-7 HAC coverage (\\%)',sprintf('%.1f',100*mean(mc[,'covers']))),
 c('Known-process coverage (\\%)',sprintf('%.1f',100*mean(mc[,'known_covers'])))),
 'Fully simulated teaching data. Each of 200 replications redraws two independent AR(1) day-shock series and multinomial actions. Normal 95\\% intervals use either fitted lag-7 Newey--West covariance or the covariance implied by the public generating process. Reader counts and the 30-day year-difference design are fixed.')

# Truth is explicit and derived from the same formula, not inferred from fitted data.
truth_total<-mean(c(.006,.010,.014)-c(.001,.003,.005))
checks<-list(seed=seed,rows_per_scenario=nrow(main),accounts=60L,
   identical_observational_counters=identical(scenarios$audience[,c('reads','likes','visible')],
                                             scenarios$recommendation[,c('reads','likes','visible')]),
   unchanged_metadata=all(vapply(scenarios,function(d)identical(d[,c('post_id','account','family','date')],
                                           panel[,c('post_id','account','family','date')]),logical(1))),
   nonnegative_counts=all(vapply(scenarios,function(d)all(d$likes>=0 & d$visible>=0 & d$likes+d$visible<=d$reads),logical(1))),
   decomposition_error=fits[[2]]$tau-fits[[3]]$tau-fits[[4]]$tau,
   true_average_total_effect=truth_total,
   estimated_average_total_effect=fits[[2]]$tau,
   true_family_recommendation_contrast=-.004,
   family_recommendation_contrast=contrast_fit$tau,
   family_contrast_standard_error=contrast_fit$standard_error)
checks$monte_carlo_coverage <- mean(mc[,'covers'])
checks$known_process_coverage <- mean(mc[,'known_covers'])
checks$family_contrast_covariance <- 'Lag-7 HAC on the directly fitted family contrast, retained for teaching comparison only'
stopifnot(checks$identical_observational_counters,checks$unchanged_metadata,
          checks$nonnegative_counts,abs(checks$decomposition_error)<1e-12)
saveRDS(list(main=fits,scenarios=list(naive=naive,difference=diff),windows=sens,
             family_contrast=contrast_fit,checks=checks),file.path(out,'data','analysis_results.rds'))
writeLines(c('# Verification of the rebuilt teaching laboratory','',
             'All data in this laboratory are generated, including account identifiers and dates.',
             '',capture.output(str(checks))),file.path(out,'verification.md'))

macros<-c(paste0('\\newcommand{\\SimTotal}{',fmt(100*fits[[2]]$tau),'}'),
          paste0('\\newcommand{\\SimLike}{',fmt(100*fits[[3]]$tau),'}'),
          paste0('\\newcommand{\\SimVisible}{',fmt(100*fits[[4]]$tau),'}'),
          paste0('\\newcommand{\\SimCalendarNaive}{',fmt(100*naive[[3]]$tau),'}'),
          paste0('\\newcommand{\\SimCalendarDiff}{',fmt(100*diff[[3]]$tau),'}'),
          paste0('\\newcommand{\\SimFamilyContrast}{',fmt(100*contrast_fit$tau),'}'),
          paste0('\\newcommand{\\SimCoverage}{',sprintf('%.1f',100*mean(mc[,'covers'])),'}'),
          paste0('\\newcommand{\\SimKnownCoverage}{',sprintf('%.1f',100*mean(mc[,'known_covers'])),'}'))
writeLines(macros,file.path(out,'tables','result_macros.tex'))

pdf(file.path(out,'figures','daily_redesign.pdf'),width=8,height=4.8,useDingbats=FALSE)
par(mar=c(4.2,4.4,.6,.7),las=1)
z<-daily(main,'total_rate');y20<-z[z$year==2020,];y19<-z[z$year==2019,]
plot(y20$x,100*y20$y,type='l',col='blue',lwd=1.5,xlab='Days from illustrative July 1 cutoff',
 ylab='Mean actions per 100 reader trials',ylim=range(100*z$y))
lines(y19$x,100*y19$y,col='black',lty=2,lwd=1.5);abline(v=0,lty=3,col='black')
legend('topleft',legend=c('2020: redesign','2019: comparison'),col=c('blue','black'),
 lty=c(1,2),bty='n');dev.off()

pdf(file.path(out,'figures','mechanism_equivalence.pdf'),width=8,height=4.8,useDingbats=FALSE)
par(mfrow=c(1,2),mar=c(4.2,4.2,1.8,.5),las=1)
for(o in c('like_rate','visible_rate')) {
 z<-daily(main[main$year==2020,],o)
 plot(z$x,100*z$y,type='l',col='black',lwd=1.5,xlab='Days from cutoff',ylab='Actions per 100 reader trials',
      main=if(o=='like_rate')'A. Count-only like' else 'B. Recommendation')
 abline(v=0,lty=3)
}
dev.off()
cat('Teaching laboratory complete. Rows per scenario:',nrow(main),'\n')
print(checks)
