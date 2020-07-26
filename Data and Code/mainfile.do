/******************************************************************************
Author: Ruru Hoong
File Name: mainfile.do
Description: Main file for running analysis on app limit intervention
******************************************************************************/

clear all

*** CHANGE INPUT SPECIFIC TO THE USER/ANALYSIS ***
gl identity "/Users/ruruhoong/Desktop/Main Experiment Data"

use "applimit.dta", clear


* Execute analysis 
do 0_generatevar
do 1_summary
do 2_analysisST 
do 3_analysisLT_HTE
do 4_robustST
do 5_robustLT



