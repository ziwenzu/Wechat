********************************************************************************
* APSR-style clustered bar chart
* 5 metrics × 3 categories, bars ordered BIG → SMALL within each metric
* Values are margins-based predicted log engagement
********************************************************************************

clear all
set more off
set scheme s1color 

* 1. Load data ---------------------------------------------------------------
use "$data/processed_data.dta", clear 

* 2. Encode category (Hard / Service / Soft) ---------------------------------
capture drop cat_num
encode category, gen(cat_num)

* 3. Generate log-transformed outcome variables ------------------------------
capture drop ln_read ln_like ln_share ln_collect ln_comment
gen ln_read    = ln(read_num       + 1)
gen ln_like    = ln(like_num       + 1)
gen ln_share   = ln(share_num      + 1)
gen ln_collect = ln(collect_num    + 1)
gen ln_comment = ln(comment_count  + 1)

* 4. Run regressions + margins, collect predicted means ----------------------
* Metrics in desired order: Reads → Likes → Shares → Collects → Comments
local dvs ln_read ln_like ln_share ln_collect ln_comment

tempname handle
tempfile plotdata

postfile `handle' str15 metric catnum double coef using `plotdata', replace

foreach dv of local dvs {
    
    di as txt "Processing `dv' ..."
    
    quietly regress `dv' i.cat_num is_headline
    quietly margins cat_num
    
    * r(b): 1×3 row vector of predicted values for cat_num
    matrix b = r(b)'
    
    * catnum = 1,2,3 (assumed Hard, Service, Soft in encode order)
    forvalues c = 1/3 {
        scalar coef_ = b[`c',1]
        post `handle' ("`dv'") (`c') (coef_)
    }
}

postclose `handle'

* 5. Load the collected margins-based predicted means ------------------------
use `plotdata', clear

* 6. Category labels ---------------------------------------------------------
label define catlbl 1 "Hard" 2 "Service" 3 "Soft", replace
label values catnum catlbl

* 7. Metric ordering: Reads → Likes → Shares → Collects → Comments -----------
gen metric_id = .
replace metric_id = 1 if metric == "ln_read"
replace metric_id = 2 if metric == "ln_like"
replace metric_id = 3 if metric == "ln_share"
replace metric_id = 4 if metric == "ln_collect"
replace metric_id = 5 if metric == "ln_comment"

label define metriclbl ///
    1 "Reads"    ///
    2 "Likes"    ///
    3 "Shares"   ///
    4 "Collects" ///
    5 "Comments", replace
label values metric_id metriclbl

* 8. Within-metric sorting: BIG → SMALL (left → right) -----------------------
* Sort by metric, then descending coef
gsort metric_id -coef

* rank within each metric:
*   rank = 1 → largest
*   rank = 2 → middle
*   rank = 3 → smallest
by metric_id: gen rank = _n

* Generate x positions:
*   rank 1 (largest)   → left:  metric_id - 0.25
*   rank 2 (middle)    → center: metric_id
*   rank 3 (smallest)  → right: metric_id + 0.25
gen x = .
replace x = metric_id - 0.25 if rank == 1
replace x = metric_id         if rank == 2
replace x = metric_id + 0.25 if rank == 3

* 9. APSR-style color palette -----------------------------------------------
* Hard    = dark gray
* Service = deep blue (highlight group)
* Soft    = light desaturated blue
local col_hard    "90 90 90"
local col_service "33 74 128"
local col_soft    "160 185 210"

* 10. Draw APSR-style clustered bar chart (NO error bars) --------------------
twoway ///
    (bar coef x if catnum == 1, barwidth(0.22) color("`col_hard'")) ///
    (bar coef x if catnum == 2, barwidth(0.22) color("`col_service'")) ///
    (bar coef x if catnum == 3, barwidth(0.22) color("`col_soft'")) , ///
    ///
    xlabel(1 "Reads" 2 "Likes" 3 "Shares" 4 "Collects" 5 "Comments", ///
           noticks labsize(medium)) ///
    ylabel(, labsize(medium)) ///
    legend(order(1 "Hard" 2 "Service" 3 "Soft") rows(1) size(medsmall)) ///
    graphregion(color(white)) ///
    ytitle("Estimated log engagement level", size(medium)) ///
    xtitle("") ///
    title("", size(medium)) ///
    name(APSR_Cluster_Sorted, replace)

* 11. Export figure ----------------------------------------------------------
graph export "$path/outputs/engagement.pdf", replace

********************************************************************************
* End
********************************************************************************
