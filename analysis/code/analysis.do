/* ==============================================================================
Project:        Authoritarian Visibility & Engagement (RDD Study)
File Name:      02_analysis.do
Description:    Performs Descriptive Statistics, Mechanism Tests (Spillover/
                Crowding-out), and RDD Analysis with Visualizations.
============================================================================== */


* ==============================================================================
* PART 5: Matrix-Based Clustered Bar Plot (Final Version)
* ==============================================================================
* 目标：实现“先指标（Reads/Likes/Shares），后分组（H/S/S）”的簇状图。
* 方法：Margins -> Matrix Export -> Coefplot.


* ==============================================================================
* PART 1: Descriptive Statistics & Validation
* ==============================================================================

global vars "read_num like_num share_num look_num comment_count collect_num"

sum $vars 
correlate $vars 

* 1. Content Stratification (Expectation: Service > Soft > Hard)
tabstat read_num like_num share_num, by(rdd_cat) statistics(mean N) format(%9.0f)

* 2. Headline Effect
tabstat read_num like_num, by(is_headline) statistics(mean) format(%9.0f)

* 3. Look Feature Availability (Validity Check for Fuzzy RDD)
local c2020 = td(01jul2020)
gen period_check = (publish_date >= `c2020')
label define p_lab 0 "Pre-2020" 1 "Post-2020"
label values period_check p_lab
tabstat look_num, by(period_check) statistics(N mean min max) format(%9.0f)
drop period_check

* ==============================================================================
* PART 2: Mechanism Tests - The "Attention Dilemma"
* ==============================================================================
* We use preserve/restore to perform account-day level aggregation
* without altering the main dataset structure.

preserve
    * Create Temp Variables for Aggregation
    gen is_hard = (rdd_cat == "Hard")
    gen hard_read_temp = read_num * is_hard
    gen service_read_temp = read_num * (rdd_cat == "Service")

    * Create Historical Service Ratio (Long-term Vitality Metric)
    sort account_id publish_date
    bysort account_id (publish_date): gen cum_service = sum(rdd_cat == "Service")
    bysort account_id (publish_date): gen cum_total = _n
    gen hist_service_ratio = (cum_service / cum_total) 

    * Collapse to Account-Day Level
    collapse (sum) daily_hard=hard_read_temp daily_service=service_read_temp ///
             (mean) hist_service_ratio, by(account_id publish_date)
    
    * Log Transformation
    gen ln_hard = ln(daily_hard + 1)
    gen ln_service = ln(daily_service + 1)
    
    * Filter: Days where propaganda was actually published
    keep if daily_hard > 0
    
    * Regression: Short-term Crowding-out vs. Long-term Vitality
    display "--- REGRESSION: The Attention Dilemma ---"
    reghdfe ln_hard ln_service hist_service_ratio, ///
        absorb(account_id publish_date) cluster(account_id)
        
    display ">>> Hypothesis Check: Short-term Coef (ln_service) should be NEGATIVE"
    display ">>> Hypothesis Check: Long-term Coef (hist_service_ratio) should be POSITIVE"
restore

* ==============================================================================
* PART 3: RDD Analysis & Visualizations
* ==============================================================================
local c2017 = td(17may2017)
local c2020 = td(01jul2020)
* --- GRAPH SET 1: 2017 Visibility Shock (Suppression) ---
* A1. Hard Propaganda (Treatment: Like)
rdplot like_num publish_date if rdd_cat == "Hard" & abs(publish_date - `c2017') < 360, ///
    c(`c2017') p(1) nbins(30 30) ci(95) /// 
    graph_options(title("A1. Hard Propaganda (Treatment)") ///
    ytitle("Avg. Likes") xtitle("Date (2017)") legend(off) name(g_2017_hard, replace))

* A2. Public Service (Placebo: Like)
rdplot like_num publish_date if rdd_cat == "Service" & abs(publish_date - `c2017') < 360, ///
    c(`c2017') p(1) nbins(30 30) ci(95) ///
    graph_options(title("A2. Public Service (Placebo)") ///
    ytitle("Avg. Likes") xtitle("Date (2017)") legend(off) name(g_2017_service, replace))

* --- GRAPH SET 2: 2020 Privacy Return (Rebound) ---
* B1. Hard Propaganda (Rebound: Like)
rdplot like_num publish_date if rdd_cat == "Hard" & abs(publish_date - `c2020') < 360, ///
    c(`c2020') p(1) nbins(30 30) ci(95) ///
    graph_options(title("B1. Hard Propaganda (Rebound)") ///
    ytitle("Avg. Likes") xtitle("Date (2020)") legend(off) name(g_2020_like, replace))

* B2. Hard Propaganda (Mechanism: Share)
* Share remains public -> No Rebound expected
rdplot share_num publish_date if rdd_cat == "Hard" & abs(publish_date - `c2020') < 360, ///
    c(`c2020') p(1) nbins(30 30) ci(95) ///
    graph_options(title("B2. Hard Propaganda (Share - No Rebound)") ///
    ytitle("Avg. Shares") xtitle("Date (2020)") legend(off) name(g_2020_share, replace))

* Combine Graphs for Paper
graph combine g_2017_hard g_2017_service g_2020_like g_2020_share, ///
    cols(2) xsize(16) ysize(12) ///
    title("The Causal Effect of Platform Visibility on Political Engagement")
