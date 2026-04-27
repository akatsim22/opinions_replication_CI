import pdb
from datetime import datetime, timedelta

import sys
sys.path.append('code/analysis/epi-model')
from models import *

lines = []
# Constants
cluster = True
reoptimize = True
begin_difference = datetime(2020, 2, 1)
maximize_difference = datetime(2020, 3, 1)
end_difference = datetime(2020, 4, 15)
end_trend = datetime(2020,5,1)
day0 = datetime(2020, 2, 1)
nperiods = 100
us_pop = 328953020
ncounties = 3141
population = round(us_pop/ncounties)

groups = {0:'yn', 1:'yv',2:'on',3:'ov'}
prop_old = 0.3216 # of 25 and up
treated_of_young = 0.0132 # so frac = 0.0143*(1-0.3216)
treated_of_old = 0.0302 # so frac = 0.0143*(0.3216)

a = 114+34+110+56
b = 94+29+69+20+67+20
c = 22+11+6+22+51+6 
d = 22+12+7+18+13+8+17+12+9 

rho_replica = np.array([[a, a, c, c], [a, a, c, c], [b, b, d, d], [b, b, d, d]])/((2*b+2*d)/4) # normalize visits to 1

gamma = np.array([1/8, 1/8, 1/8, 1/8])
ifr20_49 = 0.008
ifr50_64 = 0.015
ifr65_plus = 0.09
frac_20_49 = (0.08+0.12+0.11)
frac_50_64 = (0.15+0.13)
ifr_20_64 = frac_20_49*ifr20_49 + frac_50_64*ifr50_64 / (frac_20_49+frac_50_64)

IFR = np.array([ifr_20_64, ifr_20_64, ifr65_plus, ifr65_plus])
delta = np.array(IFR/(1-IFR)*gamma)
L = np.array([0, 0, 0, 0])
# Initial values
initN = np.array([(1-treated_of_young)*(1-prop_old), treated_of_young*(1-prop_old), (1-treated_of_old)*(prop_old), treated_of_old*(prop_old)])
initR = np.array([0,0,0,0])
initD = np.array([0,0,0,0])

magnitudes_list = {'2SLS': create_magnitudes(approach='2SLS'), 'OLS': create_magnitudes(approach='OLS')}

def create_checkpoints(betastart, betamean, betahigher, betamiddle, betaend, day0):
    checkpoints = pd.DataFrame({'period': range(-(day0-begin_difference).days,nperiods)})
    checkpoints['date'] = begin_difference + pd.to_timedelta(checkpoints['period'], 'd')
    checkpoints['betamean'] = np.nan
    checkpoints['betahigher'] = np.nan
    checkpoints.loc[(begin_difference<=checkpoints['date'])&(checkpoints['date']<=maximize_difference), 'betamean'] = np.linspace(betastart, betamean, num=(maximize_difference-begin_difference).days+1, endpoint=False)
    checkpoints.loc[(begin_difference<=checkpoints['date'])&(checkpoints['date']<=maximize_difference), 'betahigher'] = np.linspace(betastart, betahigher, num=(maximize_difference-begin_difference).days+1, endpoint=False)
    checkpoints.loc[(maximize_difference<checkpoints['date'])&(checkpoints['date']<=end_difference), 'betamean'] = np.linspace(betamean, betamiddle, num=(end_difference-maximize_difference).days, endpoint=False)
    checkpoints.loc[(maximize_difference<checkpoints['date'])&(checkpoints['date']<=end_difference), 'betahigher'] = np.linspace(betahigher, betamiddle, num=(end_difference-maximize_difference).days, endpoint=False)
    checkpoints.loc[(end_difference<checkpoints['date'])&(checkpoints['date']<=end_trend), 'betamean'] = np.linspace(betamiddle, betaend, num=(end_trend-end_difference).days, endpoint=False)
    checkpoints.loc[(end_difference<checkpoints['date'])&(checkpoints['date']<=end_trend), 'betahigher'] = checkpoints.loc[(end_difference<checkpoints['date'])&(checkpoints['date']<=end_trend), 'betamean']

    return checkpoints

def run_models(args, magnitudes_wide, rho, simulate=True):
	#offset, initcases, betastart, betamean, betahigher, betamiddle, betaend = args
	betastart, betamean, betahigher, betamiddle, betaend = args
	offset = 1
	initcases = 10

	if not (betastart >= betahigher >= betamean >= betamiddle >= betaend):
		return np.inf

	beta = np.array([betastart,betastart,betastart,betastart])
	day0_offset = day0 + pd.Timedelta(offset, 'd')

	initI = (initcases/330000000)*initN

	checkpoints_raw = create_checkpoints(betastart, betamean, betahigher, betamiddle, betaend, day0_offset)
	checkpoints = checkpoints_raw[checkpoints_raw['date']>=day0_offset]
	checkpoints = checkpoints.dropna()

	mean_viewership = create_model_dfs(nperiods=nperiods, beta=beta, L=L, gamma=gamma, delta=delta, rho=rho, 
		initN=initN, initI=initI, initR=initR, initD=initD, groups=groups, 
		checkpoints={'t': list(checkpoints['period']), 'beta': np.asarray(checkpoints[['betamean', 'betamean','betamean','betamean']])})

	higher_viewership = create_model_dfs(nperiods = nperiods, beta=beta, 
		L=L, gamma=gamma, delta=delta, rho=rho, 
		initN=initN, initI=initI, initR=initR, initD=initD, groups=groups, 
		checkpoints={'t': list(checkpoints['period']), 'beta': np.asarray(checkpoints[['betamean', 'betahigher','betamean','betahigher']])}).rename({'deaths':'deaths_higher'}, axis=1)

	combined = mean_viewership[['period','deaths']].merge(higher_viewership[['period','deaths_higher']], on='period')
	combined['date'] = (day0_offset + pd.to_timedelta(combined['period'], 'd')).dt.date

	combined = combined.merge(magnitudes_wide, on=['date'], how='left')
	combined['deaths'] = combined['deaths']*population
	combined['deaths_higher'] = combined['deaths_higher']*population
	combined['deaths'] = np.log(1+combined['deaths'])
	combined['deaths_higher'] = np.log(1+combined['deaths_higher'])
	combined['treatment'] = (combined['deaths_higher']-combined['deaths'])
	combined['treatment_2SLS'] = (combined['deaths_higher_2SLS']-combined['deaths_2SLS'])
	if simulate:
		combined = combined[(combined['date']<=end_trend.date())]
		loss1 = np.sum((combined['deaths']-combined['deaths_2SLS'])**2) + np.sum((combined['deaths_higher']-combined['deaths_higher_2SLS'])**2)
		combined = combined[(combined['date']>=datetime(2020, 3, 16).date()) & (combined['date']<=end_trend.date())]
		loss2 = np.sum(((combined['treatment']-combined['treatment_2SLS']))**2)
		print('{}, {}'.format(loss1, loss2))
		loss = loss1+400*loss2
		if loss == 0:
			pdb.set_trace()
		return loss
	else:
		combined_long = pd.melt(combined, id_vars=['period','date'])
		combined_long['type'] = 'Simulated'
		combined_long.loc[combined_long['variable'].str.contains('2SLS'), 'type'] = '2SLS'
		combined_long['variable'] = combined_long['variable'].str.replace('_2SLS','')

		higher_viewership = higher_viewership.rename({'deaths_higher': 'deaths'}, axis=1)
		differences = higher_viewership - mean_viewership
		excess_proportion = (differences['yv']+differences['ov']).iloc[-1] / differences['deaths'].iloc[-1]

		return (combined_long, checkpoints_raw, excess_proportion)

if reoptimize:
	npoints = 2500 if cluster else 10
	for approach in ['2SLS', 'OLS']:

		magnitudes, magnitudes_wide = magnitudes_list[approach]

		#for rhop in [4,3,2,1]:
		rho = rho_replica
		optimization = scipy.optimize.shgo(lambda x: run_models(x, magnitudes_wide, rho),
			#bounds=[(1, 15), (100, 2500), (0.05, 1), (0.05, 1), (0.02, 0.8), (0.01, 0.4), (0.01, 0.2)], 
			bounds=[(0.2, 1), (0.2, 1), (0.2, 0.8), (0.01, 0.4), (0.01, 0.4)], 
			options = {'ftol':1e-4},
			sampling_method='sobol',
			n=npoints)

		optimized_values = optimization['x'].astype(list)
		minima = pd.DataFrame(optimization['xl'])
		#minima.columns = ['offset','initcases','betastart', 'betamean', 'betahigher', 'betamiddle', 'betaend']
		minima.columns = ['betastart', 'betamean', 'betahigher', 'betamiddle', 'betaend']
		minima['error'] = optimization['funl']
		minima['diff'] = (minima['betahigher']**0.5-minima['betamean']**0.5)/minima['betamean']**0.5
		minima = minima.sort_values('error')
		minima.to_csv(f'data/working/epi/minima-{approach}.csv', index=False)

for approach in ['2SLS', 'OLS']:
	magnitudes, magnitudes_wide = magnitudes_list[approach]

	minima = pd.read_csv(f'data/working/epi/minima-{approach}.csv')
	#optimized_values = list(minima.iloc[0, :])
	optimized_values = list(minima.iloc[0, :5])
	#betas = optimized_values[2:7]
	betas = optimized_values[:5]

	lines.append(f'Effective {approach} R0s for young: {betas/(delta[0]+gamma[0])}')
	lines.append(f'Effective {approach} R0s for old: {betas/(delta[2]+gamma[2])}')

	rho = rho_replica
	df, checkpoints, excess_proportion = run_models(optimized_values, magnitudes_wide, rho, simulate=False)
	checkpoints['betamean'] = checkpoints['betamean'].fillna(method='backfill').fillna(method='ffill')
	checkpoints['betahigher'] = checkpoints['betahigher'].fillna(method='backfill').fillna(method='ffill')
	'''
	plot =  sns.lineplot(x='date',y='value', style='type', hue='variable', data= df[df['variable']!='treatment'])
	plt.clf()
	plot2 =  sns.lineplot(x='date',y='value', style='type', hue='variable', data= df[df['variable']=='treatment'])
	'''
	df['date'] = df['date'].astype(str)
	checkpoints['date'] = checkpoints['date'].astype(str)
	df.to_stata(f'data/working/epi/epi-deaths-{approach}.dta', write_index=False)
	checkpoints.to_stata(f'data/working/epi/epi-betas-{approach}.dta', write_index=False)
	lines.append(f'Excess proportion of deaths due to compliers ({approach}): {excess_proportion}')

with open('output/simulations.txt', 'w') as f:
	f.writelines([l+'\n \n' for l in lines])