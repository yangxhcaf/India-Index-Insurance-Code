# use read_data.R to write out data for this
# SETUP STEPS
#conda create -n gcc-hls python=3.4     # don't import pandas seems to conflict with tsfresh install? 
#conda install -n gcc-hls OTHER PACKAGES
#source activate gcc-hls
#pip install tsfresh sklearn   # installs into gcc-hls because activated
# Can't use with rodeo b/c of conflict with pandas version 

import pandas as pd
import tsfresh as ts
import statsmodels.api as smf
import os
os.chdir('/home/mmann1123/Documents/India-Index-Insurance-Code/GCC_HLS_project')

# read in data
HLS_EVI = pd.read_csv('./Data/HLS_EVI.csv').dropna()
HLS_EVI_Y=HLS_EVI['evi']
HLS_EVI.dtypes
CM_GCC = pd.read_csv('./Data/cropmonitor_merged.csv').dropna()

# get yeilds with userid as index
CM_GCC_Y = CM_GCC[['yield','uniqueuserid']].drop_duplicates()
CM_GCC_Y = CM_GCC_Y.set_index('uniqueuserid' )
CM_GCC_Y = CM_GCC_Y.ix[:,0]   # convert to series for select_features() function
 
 
list(CM_GCC)

# extract features
FT_HLS_EVI = ts.extract_features(HLS_EVI, column_id="uniqueuserid", column_sort="time",column_value='evi')
FT_CM_GCC = ts.extract_features(CM_GCC, column_id="uniqueuserid", column_sort="datetime",column_value='gcc_90')


# impute NAs
from tsfresh.utilities.dataframe_functions import impute
FT_CM_GCC = impute(FT_CM_GCC)
FT_HLS_EVI = impute(FT_HLS_EVI)

FT_HLS_EVI = FT_HLS_EVI[FT_HLS_EVI.index.isin(CM_GCC_Y.index)]  # make sure one is a subset of the other by site id 
CM_GCC_Y = CM_GCC_Y[CM_GCC_Y.index.isin(FT_HLS_EVI.index)]


# find most relevant features
from tsfresh import select_features
FF_CM_GCC = select_features(FT_CM_GCC, CM_GCC_Y,fdr_level=4)
FF_CM_GCC.shape

FF_HLS_EVI = select_features(FT_HLS_EVI, CM_GCC_Y,fdr_level=4)
FF_HLS_EVI.shape

FF_GCC_EVI = select_features( pd.concat([FT_CM_GCC, FT_HLS_EVI], axis=1),CM_GCC_Y, fdr_level=2) # for joint features
FF_GCC_EVI.shape


******************************
# run regression
FF_CM_GCC.sort_index(inplace=True)
FF_HLS_EVI.sort_index(inplace=True)
CM_GCC_Y.sort_index(inplace=True)
FF_GCC_EVI.sort_index(inplace=True)

# add a constant 
FF_CM_GCC['Constant'] = 1 
FF_HLS_EVI['Constant'] = 1 
FF_GCC_EVI['Constant'] = 1 



##########  GCC REGRESSIONS
model = smf.OLS(CM_GCC_Y, FF_CM_GCC).fit()
predictions = model.predict(FF_CM_GCC) # make the predictions by the model

# Print out the statistics
model.summary()

# join Y and X 
result = pd.concat([CM_GCC_Y, FF_CM_GCC], axis=1)


SB_CM_GCC = FF_CM_GCC[['Constant','gcc_90__change_quantiles__f_agg_"mean"__isabs_True__qh_0.4__ql_0.2',
	'gcc_90__change_quantiles__f_agg_"var"__isabs_False__qh_0.4__ql_0.2',
	'gcc_90__agg_linear_trend__f_agg_"max"__chunk_len_10__attr_"stderr"',
	'gcc_90__fft_coefficient__coeff_8__attr_"abs"',
	'gcc_90__change_quantiles__f_agg_"mean"__isabs_True__qh_0.6__ql_0.2',
	'gcc_90__change_quantiles__f_agg_"var"__isabs_False__qh_0.6__ql_0.2',
	'gcc_90__energy_ratio_by_chunks__num_segments_10__segment_focus_6',
	'gcc_90__energy_ratio_by_chunks__num_segments_10__segment_focus_5',
	'gcc_90__change_quantiles__f_agg_"var"__isabs_True__qh_0.6__ql_0.2'
]]

model = smf.OLS(CM_GCC_Y, SB_CM_GCC).fit()
model.summary()
 
# check varience inflation factors 
from statsmodels.stats.outliers_influence import variance_inflation_factor
[variance_inflation_factor(SB_CM_GCC.values, i) for i in range(SB_CM_GCC.shape[1])]


SB_CM_GCC = FF_CM_GCC[['Constant','gcc_90__change_quantiles__f_agg_"mean"__isabs_True__qh_0.4__ql_0.2',
	'gcc_90__change_quantiles__f_agg_"var"__isabs_False__qh_0.4__ql_0.2',
	'gcc_90__agg_linear_trend__f_agg_"max"__chunk_len_10__attr_"stderr"',
	'gcc_90__fft_coefficient__coeff_8__attr_"abs"',
	'gcc_90__change_quantiles__f_agg_"mean"__isabs_True__qh_0.6__ql_0.2',
	'gcc_90__change_quantiles__f_agg_"var"__isabs_False__qh_0.6__ql_0.2',
	 
	
	'gcc_90__change_quantiles__f_agg_"var"__isabs_True__qh_0.6__ql_0.2'
]]

model = smf.OLS(CM_GCC_Y, SB_CM_GCC).fit()
model.summary()
[variance_inflation_factor(SB_CM_GCC.values, i) for i in range(SB_CM_GCC.shape[1])]





##########  EVI REGRESSIONS
model = smf.OLS(CM_GCC_Y, FF_HLS_EVI).fit()
 
# Print out the statistics
model.summary()
 

SB_HLS_EVI = FF_HLS_EVI[['Constant','evi__fft_coefficient__coeff_51__attr_"abs"',
	'evi__change_quantiles__f_agg_"mean"__isabs_False__qh_0.6__ql_0.0',
	'evi__ratio_beyond_r_sigma__r_0.5',
	'evi__change_quantiles__f_agg_"mean"__isabs_False__qh_0.4__ql_0.2',
	'evi__fft_coefficient__coeff_39__attr_"real"',
	'evi__fft_coefficient__coeff_18__attr_"imag"'
]]

model = smf.OLS(CM_GCC_Y, SB_HLS_EVI).fit()
model.summary()
 
# check varience inflation factors 
from statsmodels.stats.outliers_influence import variance_inflation_factor
[variance_inflation_factor(SB_HLS_EVI.values, i) for i in range(SB_HLS_EVI.shape[1])]


######### JOINT REGRESSION

# BY JOINT FEATURE SELECTION 
#model = smf.OLS(CM_GCC_Y, FF_GCC_EVI).fit()
#model.summary()


# BY JOINING BOTH MODELS 
# Join two subsets together and run regression, does GCC add value? 

GCC_EVI = pd.concat([SB_CM_GCC,   SB_HLS_EVI.drop('Constant', 1)], axis=1)

model = smf.OLS(CM_GCC_Y, GCC_EVI).fit()
 
# Print out the statistics
model.summary()

SB_GCC_EVI = GCC_EVI[['Constant',
	'gcc_90__change_quantiles__f_agg_"mean"__isabs_True__qh_0.4__ql_0.2',
	'gcc_90__change_quantiles__f_agg_"var"__isabs_False__qh_0.4__ql_0.2',
	'gcc_90__agg_linear_trend__f_agg_"max"__chunk_len_10__attr_"stderr"', 
	'gcc_90__change_quantiles__f_agg_"mean"__isabs_True__qh_0.6__ql_0.2',
	'gcc_90__change_quantiles__f_agg_"var"__isabs_False__qh_0.6__ql_0.2',	
	'gcc_90__change_quantiles__f_agg_"var"__isabs_True__qh_0.6__ql_0.2',
	'evi__fft_coefficient__coeff_51__attr_"abs"',
	'evi__change_quantiles__f_agg_"mean"__isabs_False__qh_0.6__ql_0.0',
	'evi__ratio_beyond_r_sigma__r_0.5',
	'evi__change_quantiles__f_agg_"mean"__isabs_False__qh_0.4__ql_0.2',
	'evi__fft_coefficient__coeff_39__attr_"real"',
	'evi__fft_coefficient__coeff_18__attr_"imag"'
]]

model = smf.OLS(CM_GCC_Y, SB_GCC_EVI).fit()
model.summary()

# check varience inflation factors 
from statsmodels.stats.outliers_influence import variance_inflation_factor
[variance_inflation_factor(SB_HLS_EVI.values, i) for i in range(SB_HLS_EVI.shape[1])]






#from sklearn.pipeline import Pipeline
#from sklearn.ensemble import RandomForestClassifier
#from tsfresh.examples import load_robot_execution_failures
#from tsfresh.transformers import RelevantFeatureAugmenter

#pipeline_GCC = Pipeline([('augmenter', RelevantFeatureAugmenter(column_id='uniqueuserid', column_sort='time',column_value='gcc_90')),
 #           ('classifier', RandomForestClassifier())])

#timeseries, y = load_robot_execution_failures()
# setup empty container for extracted features
#X = pd.DataFrame(index=CM_GCC_Y.index)
# add features
#pipeline_GCC.set_params(augmenter__timeseries_container=CM_GCC)
# run feature selection and randomforest
#pipeline_GCC.fit(X, CM_GCC_Y)



# deactivate environment 
source deactivate 
