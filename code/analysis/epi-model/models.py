import numpy as np
import pandas as pd
import scipy as scipy
import scipy.integrate
from itertools import chain


nu = 2
class SIRDModel():
    """
    A class to simulate the Acemoglu et al MR-SIR model
    ===================================================
    Params: beta    Rate of transmission (exposure) 
            L       Proportion in lockdown
            gamma   Rate of recovery (upon infection) 
            delta   Rate of infection-related death
            
            initI   Init number of infectious individuals      
            initR   Init number of recovered individuals     
            initD   Init number of infection-related fatalities
                    (all remaining nodes initialized susceptible)   
    """

    def __init__(self, beta, L, gamma, delta, rho, initN, initI, initR, initD):

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Model Parameters:
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        self.beta   = beta
        self.L      = L
        self.gamma  = gamma 
        self.delta  = delta
        self.rho    = rho
        assert beta.shape[0] == L.shape[0] == gamma.shape[0] == delta.shape[0], 'Parameters do not have the same number of groups.'
        self.ngroups = beta.shape[0]

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Initialize Timekeeping:
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        self.t       = 0
        self.tmax    = 0 # will be set when run() is called
        self.tseries = np.array([0])
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Initialize Counts of inidividuals with each state:
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        self.numN       = initN

        self.numI       = initI
        self.numR       = initR
        self.numD       = initD
        self.numS       = self.numN-self.numI-self.numR-self.numD

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    @staticmethod
    def system_dfes(t, variables, beta, L, gamma, delta, rho):
        #print('t={}'.format(t))
        beta = beta**0.5
        ngroups = beta.shape[0]

        S = variables[0:ngroups]
        assert (S >= 0).all()
        I = variables[ngroups:(2*ngroups)]
        #assert (I >= 0).all()
        R = variables[(2*ngroups):(3*ngroups)]
        assert (R >= 0).all()
        D = variables[(3*ngroups):(4*ngroups)]
        assert (D >= 0).all()

        assert round(np.sum(S+I+R+D), 5) == 1

        deltas = []

        dSs = [0,0,0,0]
        dIs = [0,0,0,0]
        dRs = [0,0,0,0]
        dDs = [0,0,0,0]

        for j in range(ngroups):
            '''
            # \\dot{I}_j &= \beta_j (1- L_j) S_j \\sum_k \rho_{jk} \\beta_k (1- L_k) I_k - \\gamma_j I_j - \\delta_j I_j 
            \\dot{D}_j &= \\delta_j I_j \\
            \\dot{R}_j &= \\gamma_j I_j \\
            \\dot{S}_j &= -\\dot{I}_j - \\dot{R}_j - \\dot{D}_j
            1 &= \\sum_j \\left(S_j + I_j + R_j + D_j \\right) 
            '''
            def jb(beta1, beta2):
                minb = min([beta1, beta2])
                maxb = max([beta1, beta2])
                return minb**(2-nu) * maxb**nu

            summand = S[j]*np.sum([jb(beta[j], beta[k]) * (1-L[j]) * (1-L[k]) * rho[j, k] * I[k] for k in range(ngroups)])
            dIs[j] =  summand  - (gamma[j]+delta[j])*I[j] 
            #print('New I: {}'.format(I[j]-dIs[j]))
            dDs[j] = (delta[j]*I[j])
            dRs[j] = gamma[j]*I[j]
            assert [d>=0 for d in [dDs[j], dRs[j]]] #deaths and recovered can never decrease
            dSs[j] = (- (dIs[j] + dDs[j] + dRs[j]))

        return list(chain.from_iterable([dSs, dIs, dRs, dDs]))


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    def run_epoch(self, runtime, dt=1):
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Create a list of times at which the ODE solver should output system values.
        # Append this list of times as the model's timeseries
        t_eval    = np.arange(start=dt, stop=runtime, step=dt)
        # Define the range of time values for the integration:
        t_span          = (dt, runtime)
        # Define the initial conditions as the system's current state:
        # (which will be the t=0 condition if this is the first run of this model, 
        # else where the last sim left off)

        if len(self.numS.shape) == 1:
            conds = [self.numS.tolist(), self.numI.tolist(), self.numR.tolist(), self.numD.tolist()]
        else:
            conds = [self.numS[-1].tolist(), self.numI[-1].tolist(), self.numR[-1].tolist(), self.numD[-1].tolist()]

        init_cond       = list(chain.from_iterable(conds))

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Solve the system of differential eqns:
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        
        solution        = scipy.integrate.solve_ivp(
            lambda t, X: SIRDModel.system_dfes(t, X, self.beta, self.L, self.gamma, self.delta, self.rho), 
            t_span=t_span, 
            y0=init_cond, 
            method='RK45',
            max_step=0.1,
            t_eval=t_eval)
        '''
        solution        = scipy.integrate.odeint(
            lambda t, X: SIRDModel.system_dfes(t, X, self.beta, self.L, self.gamma, self.delta, self.rho), 
            y0=init_cond,
            tfirst=True,
            t=t_eval)
        '''
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Store the solution output as the model's time series and data series:
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        self.tseries    = np.append(self.tseries, solution['t']+self.t)
        solution['y'] = solution['y'].transpose()

        if len(self.numS.shape)==1:
            self.numS = solution['y'][:,0:self.ngroups]
            self.numI = solution['y'][:,(self.ngroups):(2*self.ngroups)]
            self.numR = solution['y'][:,(2*self.ngroups):(3*self.ngroups)]
            self.numD = solution['y'][:,(3*self.ngroups):(4*self.ngroups)]
        else:
            self.numS       = np.concatenate((self.numS, solution['y'][:,0:self.ngroups]), axis=0)
            self.numI       = np.concatenate((self.numI, solution['y'][:,(self.ngroups):(2*self.ngroups)]), axis=0)
            self.numR       = np.concatenate((self.numR, solution['y'][:,(2*self.ngroups):(3*self.ngroups)]), axis=0)
            self.numD       = np.concatenate((self.numD, solution['y'][:,(3*self.ngroups):(4*self.ngroups)]), axis=0)

        self.t = self.tseries[-1]

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    def run(self, T, dt=0.1, checkpoints=None, verbose=False):

        if(T>0):
            self.tmax += T
        else:
            return False
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Pre-process checkpoint values:
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(checkpoints):
            numCheckpoints = len(checkpoints['t'])
            paramNames = ['beta','L','gamma','delta', 'rho']
            for param in paramNames:
                # For params that don't have given checkpoint values (or bad value given), 
                # set their checkpoint values to the value they have now for all checkpoints.
                if(param not in list(checkpoints.keys())
                    or not isinstance(checkpoints[param], (list, np.ndarray)) 
                    or len(checkpoints[param])!=numCheckpoints):
                    checkpoints[param] = [getattr(self, param)]*numCheckpoints
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        # Run the simulation loop:
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        if(not checkpoints):
            self.run_epoch(runtime=self.tmax, dt=dt)

            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            if(verbose):
                print("\t S   = " + str(self.numS[-1]))
                print("\t I   = " + str(self.numI[-1]))
                print("\t R   = " + str(self.numR[-1]))
                print("\t D   = " + str(self.numD[-1]))
                    
        else: # checkpoints provided
            for checkpointIdx, checkpointTime in enumerate(checkpoints['t']):
                # Run the sim until the next checkpoint time:
                self.run_epoch(runtime=checkpointTime-self.t, dt=dt)
                # Having reached the checkpoint, update applicable parameters:
                if verbose:
                    print("[Checkpoint: Updating parameters]")
                for param in paramNames:
                    setattr(self, param, checkpoints[param][checkpointIdx])
                  
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                if(verbose):
                    print("\t S   = " + str(self.numS[-1]))
                    print("\t I   = " + str(self.numI[-1]))
                    print("\t R   = " + str(self.numR[-1]))
                    print("\t D   = " + str(self.numD[-1]))

            if(self.t < self.tmax):
                self.run_epoch(runtime=self.tmax-self.t, dt=dt)

        return True

def create_model_dfs(nperiods, beta, L, gamma, delta, rho, initN, initI, initR, initD, groups, checkpoints):
    model = SIRDModel(beta=beta, L=L, gamma=gamma, delta=delta, rho=rho, initN=initN, initI=initI, initR=initR, initD=initD)

    model.run(T=nperiods, checkpoints=checkpoints)
    deaths = pd.DataFrame(model.numD, columns = range(model.numD.shape[1]))
    deaths = deaths.rename(groups, axis=1)
    assert deaths.shape[0] == model.tseries[1:].shape[0]
    deaths['period'] = model.tseries[1:]
    deaths['deaths'] = np.sum(deaths.drop('period', axis=1), axis=1)
    deaths = deaths[deaths['period'].round() == deaths['period'].round(2)].reset_index(drop=True)
    return deaths

def create_magnitudes(approach):
    magnitudes = pd.read_stata('data/working/epi/magnitudes-{}.dta'.format(approach))
    magnitudes = magnitudes[magnitudes.type=='deaths']
    magnitudes = magnitudes.drop(['type','error_more_higher','error_more_lower','alpha'],axis=1)
    magnitudes = magnitudes.rename({'value':'log_real'}, axis=1)
    magnitudes = magnitudes[magnitudes['date']<=pd.datetime(2020, 5, 2)]
    magnitudes['level_real'] = (np.exp(magnitudes['log_real'])-1)
    magnitudes = magnitudes.rename({'key':'variable', 'log_real': 'value'}, axis=1)
    magnitudes_wide = magnitudes[['date','variable','value']].pivot(index='date',columns='variable',values='value')
    #magnitudes_wide.columns = magnitudes_wide.columns.astype(list)
    magnitudes_wide.columns = magnitudes_wide.columns.tolist()
    magnitudes_wide = magnitudes_wide.reset_index().rename({'Mean viewership difference':'deaths_2SLS','1 SD higher viewership difference':'deaths_higher_2SLS'}, axis=1)
    magnitudes_wide['date'] = magnitudes_wide['date'].dt.date
    magnitudes['type'] = '2SLS'
    return [magnitudes, magnitudes_wide]

