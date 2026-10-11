# Presentation-only regeneration from saved, unchanged model estimates.
# This file is also sourced by the full observational reanalysis.
if(!exists('out'))out<-file.path(getwd(),'analysis/output/observational')
z<-read.csv(file.path(out,'data/temporal_sensitivity.csv'))
fmt40<-function(v)sprintf('%.3f',v)
star40<-function(b,e){p<-2*pnorm(-abs(b/e));ifelse(p<.01,'$^{**}$',ifelse(p<.05,'$^{*}$',''))}
lines40<-c('\\begin{table}[H]','\\centering\\setstretch{1}\\footnotesize',
 '\\setlength{\\tabcolsep}{6pt}\\setlength{\\abovecaptionskip}{0pt}',
 '\\caption{Window and covariance sensitivity of the year contrast}\\label{tab:obssensitivity}',
 '\\begin{threeparttable}\\begin{tabular}{lccc}\\toprule',
 ' & (1) & (2) & (3) \\\\','HAC lag & 3 & 7 & 14 \\\\','\\midrule')
row40<-function(v)paste0(paste(v,collapse=' & '),' \\\\')
for(i in seq_along(c(14,30,45))){
 h40<-c(14,30,45)[i];a40<-z[z$h==h40,];a40<-a40[match(c(3,7,14),a40$lag),]
 lines40<-c(lines40, paste0('\\multicolumn{4}{l}{\\textit{',LETTERS[i],'. ',h40,'-day window}} \\\\'),
 row40(c('Post-cutoff (pp)',paste0(fmt40(100*a40$b),star40(a40$b,a40$error)))),
 row40(c('',paste0('(',fmt40(100*a40$error),')'))),
 row40(c('$N$',a40$N)),row40(c('$R^2$',fmt40(a40$r2))), '\\midrule')
}
lines40<-c(lines40,row40(c('Additional controls',rep('No',3))),row40(c('Local linear trends',rep('Yes',3))),
 '\\bottomrule\\end{tabular}',
 '\\renewcommand{\\TPTnoteSettings}{\\leftmargin0pt\\labelwidth0pt\\labelsep0pt\\topsep0pt\\partopsep0pt\\itemsep0pt\\parsep0pt}',
 '\\begin{tablenotes}\\footnotesize\\setstretch{1}\\justifying',
 '\\item[] Notes: Paired 2020 minus 2019 daily total rates, with triangular weights. Parentheses contain fitted Newey--West standard errors. Window and covariance lag vary across panels and columns. * $p<0.05$, ** $p<0.01$.',
 '\\end{tablenotes}\\end{threeparttable}\\end{table}')
writeLines(lines40,file.path(out,'tables','temporal_sensitivity.tex'))
