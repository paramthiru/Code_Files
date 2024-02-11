/*==============================================================================
Stata version 17.0

Date: 20/03/2023

Creator: Darshana Anandan

Task: Econometrics Project (Replication of the paper by Altindag et al)

Description: Relationship between Mental health and Social Isolation
1)  Employing Regression Discontinuity to study how COVID-19 lockdown affected 
    mental health.
2)  Exploring 3 potential sources of mental distress - employment & income, 
    social isolation and household conflicts
3)  Using COVID-19 lockdown induced social isolation as an Instrumental Variable 
    to assess the relationship between mental health and loneliness.

==============================================================================*/

clear all
set more off, perm
cap log close

// Set the directories & environment
local input "D:\Stata_ECON_Project\MasterData"
local output "D:\Stata_ECON_Project\Output"

log using "`output'\ECON_Project_LogFile", replace text

use "`input'\project_mentalhealth.dta", clear
cd "`output'"
describe

// Remove entries with no responses
drop if non_response == 1

//==============================================================================

// Table 1: Summary Statistics for 59-70 year-old-individuals

summarize highschool illiterate female married widowed_separated non_turk ///
    pre_covid_hhsize psych_support chronic_disease outside_week under_curfew ///
    never_out paid_employment total_employment job_to_return money_as_usual ///
    money_distressed lim_social_interaction limited_physical_act hh_size ///
    conflict /// 
    if age >= 59 & age <= 70

//==============================================================================

// Table 2: List of SRQ-20 Questions and their Summary Statistics

summarize head_ache mal_appetite sleeplessness scared shaking nervous ///
    indigestion unfocused unhappy weepy unwillingness undecisiveness ///
    disrupted useless uninterest worthless suicidal usually_tired ///
    stomach_discomfort quickly_tired ///
    if age >= 59 & age <= 70
    
//==============================================================================

// Regression Discontinuity
// Creates regression table for each dependent variables.
// Each table lists results for 4 different range of the running variable (dif).

local depend_vars ///
    outside_week under_curfew never_out ///
    z_depression z_somatic z_nonsomatic sum_srq ///
    paid_employment total_employment job_to_return money_as_usual ///
        money_distressed ///
    lim_social_interaction limited_physical_act ///
    hh_size conflict
local months_diff_list 17 30 45 60

eststo clear
capture noisily {
    erase tables_output.rtf
}

foreach depend_var in `depend_vars' {
    eststo clear
    foreach months_diff in `months_diff_list' {
        eststo: reg `depend_var' before1955 dif i.province_n i.month ///
            i.survey_taker_id i.education female i.ethnicity ///
            before1955#c.dif /// 
            if dif <= `months_diff' & dif >= -`months_diff', cluster(dif)
    }
    esttab _all using tables_output.rtf, ///
        se keep(before1955 dif _cons) ///
        title(`: variable label `depend_var'') ///
        mtitles(±17 ±30 ±45 ±60) ///
        append
}

//==============================================================================

// Instrumental variable
// Variable 'dif' is used as an instrument to run IV regression for the 
//  dependent variables (mental distress).
// Two tables are created - with and without the independent variable 
//  (chronic disease).

local mental_distress_vars z_depression z_somatic z_nonsomatic sum_srq

eststo clear
foreach mental_distress in `mental_distress_vars' {
    eststo: ivregress 2sls `mental_distress' (never_out = before1955) ///
        dif i.province_n i.month i.survey_taker_id i.education female ///
        i.ethnicity ///
        if dif >= -45 & dif <= 45, cluster(dif)
}
esttab _all using tables_output.rtf, /// 
    se keep(never_out dif _cons) ///
    title(Instrumental variable regression) ///
    mtitles(z_depression z_somatic z_nonsomatic sum_srq) ///
    append

eststo clear

foreach mental_distress in `mental_distress_vars' {
    eststo: ivregress 2sls `mental_distress' (never_out = before1955) ///
        chronic_disease dif i.province_n i.month i.survey_taker_id ///
        i.education female i.ethnicity ///
        if dif >= -45 & dif <= 45, cluster(dif)
}
esttab _all using tables_output.rtf, ///
    se keep(never_out dif _cons) /// 
    title(Instrumental variable regression (Chronic disease included)) ///
    mtitles(z_depression z_somatic z_nonsomatic sum_srq) ///
    append

//==============================================================================

// GRAPHS AND FIGURES

//==============================================================================

// Histogram of the Running variable

histogram dif if inrange(dif, -60, 60), ///
    discrete ///
    frequency ///
    addplot(pci 0 0 80 0 lcolor(blue)) ///
    title("Histogram of Running Variable") /// 
    fcolor(gs11) lcolor(gs9) graphregion(color(white)) legend(off)

graph export graphs_output_1.png, replace 

putdocx begin
putdocx paragraph
putdocx image graphs_output_1.png
putdocx save "graphs_output.docx", replace

//==============================================================================

// Balanced Covariates - Preliminary check for Regression discontinuity
// Graphs are generated for each pre-determined covariates plotted against the 
//  running variable.

collapse before1955 highschool illiterate female married widowed_separated ///
    non_turk pre_covid_hhsize psych_support chronic_disease ///
    if inrange(dif, -44, 44), by(dif)

local covariates_list highschool illiterate female married widowed_separated ///
    non_turk pre_covid_hhsize psych_support chronic_disease
local graphs_2 ""

foreach covariates in `covariates_list' {
    reg `covariates' dif before1955 before1955#c.dif, cluster(dif)
    predict fit_`covariates'
    gen fit_`covariates'_1 = fit_`covariates' if dif<0
    gen fit_`covariates'_2 = fit_`covariates' if dif>0
    twoway ///
        (scatter `covariates' dif, color(black) name(graph_`covariates', ///
            replace)) ///
        (line fit_`covariates'_1 fit_`covariates'_2 dif, lcolor(black black) ///
            lwidth(thick thick)) ///
        if abs(dif) <= 44, ///
        xtitle("Born before 1955 December [in months]", ///
            size(huge) margin(medsmall)) ///
        title("Panel A. `covariates'", size(huge) color(black)) ///
        xline(0) legend(off) graphregion(color(white)) ///
        xlabel(, labsize(large)) ylabel(0(0.2)1, labsize(large))
    local graphs_2 "`graphs_2' graph_`covariates'"
    }
graph combine `graphs_2', col(3) xsize(25) ysize(20) iscale(*0.4) ///
    imargin(medium)
graph export graphs_output_2.png, replace 

putdocx begin
putdocx paragraph
putdocx image graphs_output_2.png
putdocx save "graphs_output.docx", replace

//==============================================================================

// Regression Discontinuity Graphs
// For each dependent variable, a graph is plotted against the running variable 
//  to show the discontinuity.
// Graphs plot residualized values of the dependent variable.

use "`input'\project_mentalhealth.dta", clear

local mental_distress_vars z_depression z_somatic z_nonsomatic sum_srq
local resid_vars ""
local predict_fit_vars ""

foreach mental_distress in `mental_distress_vars' {
    reg `mental_distress' i.province_n i.month i.survey_taker_id i.education ///
        female i.ethnicity ///
        if dif >= -44 & dif <= 44, cluster(dif)
    predict `mental_distress'_resid, residuals
    local resid_vars "`resid_vars' `mental_distress'_resid"
    reg `mental_distress'_resid dif before1955 before1955#c.dif ///
        if dif >= -44 & dif <= 44, cluster(dif)
    predict fit_`mental_distress'
    local predict_fit_vars "`predict_fit_vars' fit_`mental_distress' "
}

collapse `resid_vars' `predict_fit_vars', by(dif)

local graphs_3 ""
foreach mental_distress in `mental_distress_vars' {
    gen fit_`mental_distress'_1 = fit_`mental_distress' if dif<0
    gen fit_`mental_distress'_2 = fit_`mental_distress' if dif>0
    twoway ///
        (scatter `mental_distress'_resid dif, color(black) ///
            name(graph_`mental_distress', replace)) ///
        (line fit_`mental_distress'_1 fit_`mental_distress'_2 dif, ///
            lcolor(black black) lwidth(thick thick)) ///
        if abs(dif) <= 44, ///
        xtitle("Born before 1955 December [in months]", ///
        size(huge) margin(medsmall)) ///
        title("Panel B. `mental_distress'", ///
        size(huge) color(black)) xline(0) legend(off) ///
            graphregion(color(white)) ///
        xlabel(, labsize(large)) ylabel(-1(0.5)1,labsize(large))
    local graphs_3 "`graphs_3' graph_`mental_distress'"
}

graph combine `graphs_3', col(2) xsize(25) ysize(20) iscale(*0.4) ///
    imargin(medium)
graph export graphs_output_3.png, replace 

putdocx begin
putdocx paragraph
putdocx image graphs_output_3.png
putdocx save "graphs_output.docx", append

//==============================================================================

// Histogram generated to show the number of people suffering from chronic 
//  illness based on whether they were born before or after 1955.

use "`input'\project_mentalhealth.dta", clear

histogram chronic_disease if before1955 == 1, ///
    discrete ///
    fraction ///
    title("Histogram of Running Variable") ///
    fcolor(gs11) lcolor(gs9) legend(off)

label define cat 1 "Chronic Disease" 0 "No Chronic Disease"
label values chronic_disease cat
label define cate 1 "Before 1955" 0 "After 1955"
label values before1955 cate

graph bar (count), over(chronic_disease) over(before1955) legend(off) ///
    graphregion(color(white))

graph export graphs_output_4.png, replace
putdocx begin
putdocx paragraph
putdocx image graphs_output_4.png
putdocx save "graphs_output.docx", append

log off

//==============================================================================
