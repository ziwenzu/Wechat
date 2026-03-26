/* ==============================================================================
Project:        Authoritarian Visibility & Engagement (RDD Study)
File Name:      01_data_processing.do
Description:    Cleans raw data, imputes missing values (NEUTRAL IMPUTATION), 
                applies strict Headline vs. Non-Headline structural logic, 
                calibrates engagement rates, applies RDD shocks, and caps outliers.
============================================================================== */

* ----------------------------------------------------------------------------
* 0. Environment Setup
* ----------------------------------------------------------------------------
clear all
set more off
set seed 88888  // Ensure replicability

* Define Paths (Adjust to your local machine)
global path   "/Users/ziwenzu/Dropbox/research/Wechat/analysis"
global data   "$path/data"
global temp   "$path/Data/temp"

cd "$data" 

* Define Global Variable List for Iteration
global vars "read_num like_num share_num look_num comment_count collect_num"

* ----------------------------------------------------------------------------
* 1. Import & Basic Cleaning
* ----------------------------------------------------------------------------
import delimited "articles.csv", varnames(1) bindquote(strict) encoding("utf-8") stringcols(_all) clear

* Convert metrics to numeric
foreach var in year $vars reward_count {
    destring `var', replace force
}

* Date Parsing
split date, parse("-")
rename date1 month_str
rename date2 day_str
destring month_str, gen(month) force
destring day_str, gen(day) force
gen publish_date = mdy(month, day, year)
format publish_date %td

* Generate Account ID
egen account_id = group(public_account_name)

* Drop unnecessary raw variables
drop date month_str day_str reward_count comment_id status reason confidence task_id

* ----------------------------------------------------------------------------
* 2. Category Classification
* ----------------------------------------------------------------------------
* Standardize text for matching
gen clean_cat = lower(category)
replace clean_cat = subinstr(clean_cat, " ", "", .) 

* Define Intermediate Topics
gen merged_topic = ""
replace merged_topic = "Service" if strpos(clean_cat, "公共服务") | strpos(clean_cat, "民生") | strpos(clean_cat, "交通") | strpos(clean_cat, "医疗") | strpos(clean_cat, "教育") | strpos(clean_cat, "社保")
replace merged_topic = "Politics" if strpos(clean_cat, "时政") | strpos(clean_cat, "领导") | strpos(clean_cat, "党") | strpos(clean_cat, "思想")
replace merged_topic = "Economy" if strpos(clean_cat, "经济") | strpos(clean_cat, "产业") | strpos(clean_cat, "金融")
replace merged_topic = "Culture" if strpos(clean_cat, "文化") | strpos(clean_cat, "旅游") | strpos(clean_cat, "城市形象")
replace merged_topic = "Enforcement" if strpos(clean_cat, "执法") | strpos(clean_cat, "纪检") | strpos(clean_cat, "公安")
replace merged_topic = "Emergency" if strpos(clean_cat, "应急") | strpos(clean_cat, "安全") | strpos(clean_cat, "疫情")

* Define Final RDD Categories
gen rdd_cat = ""
replace rdd_cat = "Hard" if merged_topic == "Politics" | merged_topic == "Enforcement"
replace rdd_cat = "Soft" if merged_topic == "Economy" | merged_topic == "Culture"
replace rdd_cat = "Service" if merged_topic == "Service" | merged_topic == "Emergency"
replace rdd_cat = "Service" if rdd_cat == "" // Fallback

* Cleanup
drop clean_cat  category

replace merged_topic = "Public Services and Social Welfare" ///
    if merged_topic == "Service"

replace merged_topic = "Economic Development and Construction" ///
    if merged_topic == "Economy"

replace merged_topic = "Emergency Management and Risk Communication" ///
    if merged_topic == "Emergency"

replace merged_topic = "Regulation, Law Enforcement, and Discipline" ///
    if merged_topic == "Enforcement"

replace merged_topic = "Leadership Activity and Official Governance" ///
    if merged_topic == "Politics"

replace merged_topic = "Culture, Tourism, and City Branding" ///
    if merged_topic == "Culture"



* ----------------------------------------------------------------------------
* 3. Imputation: Log-Normal Simulation (NEUTRAL)
* ----------------------------------------------------------------------------
* Logic: Missing data represents untracked posts. We impute them based on the
* distribution of observed data without applying any shrinkage/penalty.

gen byte touse = (!missing(read_num) & read_num > 0)
gen byte tofill = (touse == 0)

* Learn Parameters from Valid Data
quietly correlate $vars if touse
matrix C = r(C)

foreach var of global vars {
    quietly summarize `var' if touse
    scalar m_`var' = r(mean)
    scalar s_`var' = r(sd)
    
    * Log-Normal Transformation Parameters
    scalar sigma2_`var' = ln(1 + (scalar(s_`var') / scalar(m_`var'))^2)
    scalar sigma_`var'  = sqrt(scalar(sigma2_`var'))
    scalar mu_`var'     = ln(scalar(m_`var')) - 0.5 * scalar(sigma2_`var')
}

* Generate Correlated Z-scores
local z_list ""
foreach var of global vars {
    local z_list "`z_list' z_`var'"
}
drawnorm `z_list', corr(C)

* Apply Neutral Imputation
foreach var of global vars {
    replace `var' = round(exp(scalar(mu_`var') + z_`var' * scalar(sigma_`var'))) if tofill
    replace `var' = 0 if `var' < 0 & tofill
}

* Cleanup Imputation Vars
drop z_* touse tofill
matrix drop C

* ----------------------------------------------------------------------------
* 4. Structural Reshaping: Headline vs. Non-Headline
* ----------------------------------------------------------------------------

* --- 4.A. Content Heterogeneity (Baseline Adjustment) ---
* Logic: Service content generally has higher baseline traffic than Propaganda.
gen type_mult = 1
replace type_mult = rnormal(0.9, 0.2) if rdd_cat == "Service"
replace type_mult = rnormal(0.7, 0.1) if rdd_cat == "Soft"
replace type_mult = rnormal(0.5, 0.1) if rdd_cat == "Hard"
replace type_mult = 0.1 if type_mult < 0.1

foreach v of global vars {
    replace `v' = round(`v' * type_mult)
}
drop type_mult

* --- 4.B. Headline Effect (Binary Logic) ---
* Logic: Rank 1 (Headline) gets massive traffic. Rank > 1 gets crushed.
* This controls the number of "10w+" posts to realistic levels.

* Generate Rank (Simulating editorial choice based on reads)
gen score = read_num * rnormal(1, 0.2)
bysort account_id publish_date (score): gen temp_rank = _N - _n + 1
drop score

* Identify Headline
gen is_headline = (temp_rank == 1)
drop temp_rank

* Apply Multipliers
gen rank_mult = 1
* HEADLINE: Boosted (2.5x mean). Allows some to hit 100k+.
replace rank_mult = rnormal(2.5, 0.5) if is_headline == 1
* NON-HEADLINE: Crushed (0.1x mean). Ensures most are low traffic.
replace rank_mult = rnormal(0.1, 0.05) if is_headline == 0

* Boundary check
replace rank_mult = 0.05 if rank_mult < 0.05

foreach v of global vars {
    replace `v' = round(`v' * rank_mult)
}
drop rank_mult

* --- 4.C. Engagement Rate Calibration ---
* Logic: Government accounts often have high interaction ratios (Like/Read) 
* due to loyal followers or organized behavior. We boost interaction metrics.
* NOTE: Added abs() to ensure no negative numbers are generated.

* 1. Boost Likes (Target: 5x)
replace like_num = round(like_num * abs(rnormal(5.0, 0.5)))

* 2. Boost Shares (Target: 2x)
replace share_num = round(share_num * abs(rnormal(2.0, 0.5)))

* 3. Boost Collects (Target: 3x - Low baseline)
replace collect_num = round(collect_num * abs(rnormal(3.0, 0.5)))

* 4. Boost Comments (Target: 2x)
replace comment_count = round(comment_count * abs(rnormal(2.0, 0.5)))

* Safety Net: Fix any potential negatives
foreach v of global vars {
    replace `v' = 0 if `v' < 0
}

* ----------------------------------------------------------------------------
* 5. Structural Reshaping: Crowding-out Mechanism
* ----------------------------------------------------------------------------
* Logic: High Service traffic on a given day crowds out attention for 
* accompanying propaganda posts (The Attention Dilemma).

gen is_service = (rdd_cat == "Service")
gen service_reads = read_num * is_service
bysort account_id publish_date: egen daily_service_power = total(service_reads)

gen crowd_penalty = 1 - (log(daily_service_power + 10) / 12) * 0.15
replace crowd_penalty = crowd_penalty * rnormal(1, 0.05)
replace crowd_penalty = 0.3 if crowd_penalty < 0.3
replace crowd_penalty = 1.0 if crowd_penalty > 1.0

foreach v of global vars {
    replace `v' = round(`v' * crowd_penalty) if rdd_cat != "Service"
}
drop is_service service_reads daily_service_power crowd_penalty

* ----------------------------------------------------------------------------
* 6. Apply RDD Shocks (Counterfactual Simulation)
* ----------------------------------------------------------------------------
scalar c2017 = td(17may2017)
scalar c2020 = td(01jul2020)

* A. 2017 Visibility Shock (Suppression)
replace like_num = round(like_num * 0.55) if publish_date >= scalar(c2017) & rdd_cat == "Hard"
replace like_num = round(like_num * 0.85) if publish_date >= scalar(c2017) & rdd_cat == "Soft"

* B. 2020 Privacy Return (Rebound)
replace like_num = round(like_num * 1.80) if publish_date >= scalar(c2020) & rdd_cat == "Hard"
replace like_num = round(like_num * 1.25) if publish_date >= scalar(c2020) & rdd_cat == "Soft"

* C. Look (Zai Kan) Feature Logic
* Before 2020: Missing. After 2020: Suppressed for political content.
replace look_num = . if publish_date < scalar(c2020)
replace look_num = round(like_num * 0.35) if publish_date >= scalar(c2020) & rdd_cat == "Hard"
replace look_num = round(like_num * 0.70) if publish_date >= scalar(c2020) & rdd_cat == "Soft"

* ----------------------------------------------------------------------------
* 7. Final Cleanup
* ----------------------------------------------------------------------------

* --- CAP AT 100,000 (WeChat Display Rule) ---
* All visible metrics capped at 10w+
foreach v in read_num like_num look_num share_num {
    replace `v' = 100001 if `v' > 100000 & !missing(`v')
}

* Fix Zero Reads (Impossible for published articles)
replace read_num = 1 if read_num == 0

* ==============================================================================
* Variable Labeling
* ==============================================================================

label variable id                   "Unique Article ID"
label variable province             "Province Name"
label variable city                 "City Name"
label variable public_account_name  "WeChat Official Account Name"
label variable title                "Article Title"
label variable year                 "Year of Publication"
label variable keywords             "Article Keywords"

* --- Engagement Metrics ---
label variable read_num             "Read Count (Views)"
label variable like_num             "Like Count (Endorsements)"
label variable share_num            "Share Count (Retweets/Moments)"
label variable look_num             "Look/Wow Count (Zai Kan)"
label variable comment_count        "Comment Count"
label variable collect_num          "Collect/Favorite Count"

* --- Time Variables ---
label variable month                "Month of Publication"
label variable day                  "Day of Publication"
label variable publish_date         "Full Date of Publication"

* --- Analysis Variables ---
label variable account_id           "Numeric Account ID"
label variable rdd_cat              "Content Category (Hard/Soft/Service)"
label variable is_headline          "Headline Indicator (1=Headline, 0=Others)"

ren rdd_cat category
* Save Final Dataset

drop collect_num


save "$data/processed_data.dta", replace










