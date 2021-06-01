import numpy as np
def get_Cl_err(L, Cl):
    f_sky = 1.0  # fraction of sky
    l_s = 480.  # filtering scale
    theta_pix = 0.0012  # rad
    sigma_pix = 16.e-6
    wbar = 1/(0.33e-15)
    B_cl = np.exp(-L*(L + 1)/l_s**2)

    err = np.sqrt(
        (2/((2*L+1)*f_sky)) * (Cl + wbar**(-1) /B_cl)**2
    )
    return err

def get_inv_covar(L,Cl_tt,Cl_ee,Cl_te,f_sky,nlev,th_fwhm):
	import numpy as np
	conv=np.pi/(60.*180.)

	nl_tt=np.power(nlev*conv,2.e0)
	nl_ee=2.e0*nl_tt
	th_fwhm_rad=th_fwhm*np.pi/(60.*180)
	fac=8.e0*np.log(2.e0)
	bfac=np.exp(L*(L+1)*np.power(th_fwhm_rad,2.e0)/fac)
	Cl_tt_obs=Cl_tt+nl_tt*bfac
	Cl_ee_obs=Cl_ee+nl_ee*bfac
	Cl_te_obs=Cl_te
	rl=(Cl_te_obs/np.sqrt(Cl_tt_obs*Cl_ee_obs))
	sl=np.sqrt(Cl_ee_obs/Cl_tt_obs)
	sl2=np.power(sl,2.e0)
	rl2=np.power(rl,2.e0)
	trsl=-2.0e0*rl*sl
	trdivsl=-2.e0 *rl/sl
	scriptN=(2*L+1)*f_sky*0.5
	det=Cl_tt_obs*Cl_ee_obs-Cl_te_obs**2.e0
	a=np.array([[sl2,rl2,trsl],[rl2,1.e0/sl2,trdivsl],[trsl,trdivsl,2.e0*(1.e0+rl2)]])
	return (scriptN/np.power(det,2.e0))*a*Cl_tt_obs*Cl_ee_obs
	
def get_inv_covar_tonly(L,Cl_tt,f_sky,nlev,th_fwhm):
	import numpy as np
	conv=np.pi/(60.*180.)
	nl_tt=np.power(nlev*conv,2.e0)
	th_fwhm_rad=th_fwhm*np.pi/(60.*180)
	fac=8.e0*np.log(2.e0)
	bfac=np.exp(L*(L+1)*np.power(th_fwhm_rad,2.e0)/fac)
	Cl_tt_obs=Cl_tt*2.*np.pi/(L*(L+1))+nl_tt*bfac
	Cl_tt_obs=Cl_tt_obs*(L*(L+1))/(2.*np.pi)
	#print(L,Cl_tt,nl_tt,bfac)

	return f_sky*(2*L+1)*0.5/np.power(Cl_tt_obs,2.e0)

def get_inv_covar_eonly(L,Cl_ee,f_sky,nlev,th_fwhm):
	import numpy as np
	conv=np.pi/(60.*180.)

	nl_ee=2.e0*np.power(nlev*conv,2.e0)
	th_fwhm_rad=th_fwhm*np.pi/(60.*180)
	fac=8.e0*np.log(2.e0)
	bfac=np.exp(L*(L+1)*np.power(th_fwhm_rad,2.e0)/fac)
	Cl_ee_obs=Cl_ee+nl_ee*bfac
	return f_sky*(2*L+1)*0.5/np.power(Cl_ee_obs,2.e0)
	
	
	
	
	