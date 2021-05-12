program search
implicit none

integer,parameter :: ntrymax = 1000
real(8) :: ombh2,ombh2result,ombh2min,ombh2max,deltaombh2
real(8) :: omch2,omch2result,omch2min,omch2max,deltaomch2
real(8) :: hubble,hubbleresult,hubblemin,hubblemax,deltahubble
real(8) :: myparameter1,myparameter1result,myparameter1min,myparameter1max,deltamyparameter1
real(8) :: myparameter2,myparameter2result,myparameter2min,myparameter2max,deltamyparameter2
real(8) :: myparameter3,myparameter3result,myparameter3min,myparameter3max,deltamyparameter3
real(8) :: myparameter4,myparameter4result,myparameter4min,myparameter4max,deltamyparameter4
real(8) :: logA,logAresult,logAmin,logAmax,deltalogA
real(8) :: ns,nsresult,nsmin,nsmax,deltans
real(8) :: thetastar,thetastarresult
real(8),parameter :: thetastarLCDM = 1.041197d0
real(8),parameter :: sigmathetastarLCDM = 0.00029d0
real(8) :: chi2,chi2result
real(8) :: r
integer :: ntry

! program random
! implicit none
! real(8) :: r
! call random_seed()
! call random_number(r)
! write(*,*) r
! end program

call random_seed()

ombh2result = 0.022436d0
ombh2 = ombh2result
ombh2min = 0.02217d0
ombh2max = 0.02269d0
deltaombh2 = 5.2d-7

omch2result = 0.11914d0
omch2 = omch2result
omch2min = 0.11743d0
omch2max = 0.12099d0
deltaomch2 = 3.56d-6

hubbleresult = 70.0d0
hubble = hubbleresult
hubblemin = 60.0d0
hubblemax = 80.0d0
deltahubble = 0.02d0

myparameter1result = 100.0d0
myparameter1 = myparameter1result
myparameter1min = 99.0d0
myparameter1max = 101.0d0
deltamyparameter1 = 0.002d0

myparameter2result = 0.5d0
myparameter2 = myparameter2result
myparameter2min = 0.3d0
myparameter2max = 0.7d0
deltamyparameter2 = 0.0004d0

myparameter3result = -6.90658d0
myparameter3 = myparameter3result
myparameter3min = -6.960
myparameter3max = -6.86d0
deltamyparameter3 = 0.0001d0

myparameter4result = 15.0d0
myparameter4 = myparameter4result
myparameter4min = 14.0d0
myparameter4max = 16.1d0
deltamyparameter4 = 0.002d0

logAresult = 3.0476d0
logA = logAresult
logAmin = 3.019d0
logAmax = 3.075d0
deltalogA = 0.000056d0

nsresult = 0.96822d0
ns = nsresult
nsmin = 0.9594d0
nsmax = 0.9742d0
deltans = 0.0000148d0

chi2result = 1.0d10
chi2 = chi2result

open(unit = 11, file = 'search-random-camb.txt')

ntry = 0
do while (ntry <= ntrymax)

call random_number(r)

ombh2 = ombh2result + deltaombh2*(2.0d0*r - 1.0d0)

if (ombh2 < ombh2min) then
ombh2 = ombh2min
end if

if (ombh2 > ombh2max) then
ombh2 = ombh2max
end if

call random_number(r)

omch2 = omch2result + deltaomch2*(2.0d0*r - 1.0d0)

if (omch2 < omch2min) then
omch2 = omch2min
end if

if (omch2 > omch2max) then
omch2 = omch2max
end if

call random_number(r)

hubble = hubbleresult + deltahubble*(2.0d0*r - 1.0d0)

if (hubble < hubblemin) then
hubble = hubblemin
end if

if (hubble > hubblemax) then
hubble = hubblemax
end if

call random_number(r)

myparameter1 = myparameter1result + deltamyparameter1*(2.0d0*r - 1.0d0)

if (myparameter1 < myparameter1min) then
myparameter1 = myparameter1min
end if

if (myparameter1 > myparameter1max) then
myparameter1 = myparameter1max
end if

call random_number(r)

myparameter2 = myparameter2result + deltamyparameter2*(2.0d0*r - 1.0d0)

if (myparameter2 < myparameter2min) then
myparameter2 = myparameter2min
end if

if (myparameter2 > myparameter2max) then
myparameter2 = myparameter2max
end if

call random_number(r)

myparameter3 = myparameter3result + deltamyparameter3*(2.0d0*r - 1.0d0)

if (myparameter3 < myparameter3min) then
myparameter3 = myparameter3min
end if

if (myparameter3 > myparameter3max) then
myparameter3 = myparameter3max
end if

call random_number(r)

myparameter4 = myparameter4result + deltamyparameter4*(2.0d0*r - 1.0d0)

if (myparameter4 < myparameter4min) then
myparameter4 = myparameter4min
end if

if (myparameter4 > myparameter4max) then
myparameter4 = myparameter4max
end if

call random_number(r)

logA = logAresult + deltalogA*(2.0d0*r - 1.0d0)

if (logA < logAmin) then
logA = logAmin
end if

if (logA > logAmax) then
logA = logAmax
end if

call random_number(r)

ns = nsresult + deltans*(2.0d0*r - 1.0d0)

if (ns < nsmin) then
ns = nsmin
end if

if (ns > nsmax) then
ns = nsmax
end if

open(unit = 12,file = './search-random-camb-params.ini')

write(12,*) "output_root = test"
write(12,*) "get_scalar_cls = T"
write(12,*) "get_vector_cls = F"
write(12,*) "get_tensor_cls = F"
write(12,*) "get_transfer   = F"
write(12,*) "do_lensing     = T"
write(12,*) "do_nonlinear = 0"
write(12,*) "l_max_scalar      = 2510"
write(12,*) "l_max_tensor      = 1500"
write(12,*) "k_eta_max_tensor  = 3000"
write(12,*) "use_physical   = T"
write(12,*) "ombh2          = ",ombh2
write(12,*) "omch2          = ",omch2
write(12,*) "omnuh2         = 0.0"
write(12,*) "omk            = 0"
write(12,*) "hubble         = ",hubble
write(12,*) "myparameter1   = ",myparameter1
write(12,*) "myparameter2   = ",myparameter2
write(12,*) "myparameter3   = ",myparameter3
write(12,*) "myparameter4   = ",myparameter4
write(12,*) "myparameter5 = 0.0"
write(12,*) "w              = -1"
write(12,*) "cs2_lam        = 1"
write(12,*) "temp_cmb           = 2.7255"
write(12,*) "helium_fraction    = 0.24"
write(12,*) "massless_neutrinos = 2.046"
write(12,*) "nu_mass_eigenstates = 1"
write(12,*) "massive_neutrinos  = 1"
write(12,*) "share_delta_neff = T"
write(12,*) "nu_mass_fractions = 1"
write(12,*) "nu_mass_degeneracies ="
write(12,*) "initial_power_num         = 1"
write(12,*) "pivot_scalar              = 0.05"
write(12,*) "pivot_tensor              = 0.05"
write(12,*) "scalar_amp(1)             = ",exp(logA)/1.0d10
write(12,*) "scalar_spectral_index(1)  = ",ns
write(12,*) "scalar_nrun(1)            = 0"
write(12,*) "scalar_nrunrun(1)         = 0"
write(12,*) "tensor_spectral_index(1)  = 0"
write(12,*) "tensor_nrun(1)            = 0"
write(12,*) "tensor_parameterization   = 1"
write(12,*) "initial_ratio(1)          = 1"
write(12,*) "reionization         = T"
write(12,*) "re_use_optical_depth = T"
write(12,*) "re_optical_depth     = 0.09"
write(12,*) "re_redshift          = 11"
write(12,*) "re_delta_redshift    = 1.5"
write(12,*) "re_ionization_frac   = -1"
write(12,*) "re_helium_redshift = 3.5"
write(12,*) "re_helium_delta_redshift = 0.5"
write(12,*) "RECFAST_fudge = 1.14"
write(12,*) "RECFAST_fudge_He = 0.86"
write(12,*) "RECFAST_Heswitch = 6"
write(12,*) "RECFAST_Hswitch  = T"
write(12,*) "initial_condition   = 1"
write(12,*) "initial_vector = -1 0 0 0 0"
write(12,*) "vector_mode = 0"
write(12,*) "COBE_normalize = F"
write(12,*) "CMB_outputscale = 7.42835025e12"
write(12,*) "transfer_high_precision = F"
write(12,*) "transfer_kmax           = 2"
write(12,*) "transfer_k_per_logint   = 0"
write(12,*) "transfer_num_redshifts  = 1"
write(12,*) "transfer_interp_matterpower = T"
write(12,*) "transfer_redshift(1)    = 0"
write(12,*) "transfer_filename(1)    = transfer_out.dat"
write(12,*) "transfer_matterpower(1) = matterpower.dat"
write(12,*) "transfer_power_var = 7"
write(12,*) "scalar_output_file = scalCls.dat"
write(12,*) "vector_output_file = vecCls.dat"
write(12,*) "tensor_output_file = tensCls.dat"
write(12,*) "total_output_file  = totCls.dat"
write(12,*) "lensed_output_file = lensedCls.dat"
write(12,*) "lensed_total_output_file  =lensedtotCls.dat"
write(12,*) "lens_potential_output_file = lenspotentialCls.dat"
write(12,*) "FITS_filename      = scalCls.fits"
write(12,*) "do_lensing_bispectrum = F"
write(12,*) "do_primordial_bispectrum = F"
write(12,*) "bispectrum_nfields = 1"
write(12,*) "bispectrum_slice_base_L = 0"
write(12,*) "bispectrum_ndelta=3"
write(12,*) "bispectrum_delta(1)=0"
write(12,*) "bispectrum_delta(2)=2"
write(12,*) "bispectrum_delta(3)=4"
write(12,*) "bispectrum_do_fisher= F"
write(12,*) "bispectrum_fisher_noise=0"
write(12,*) "bispectrum_fisher_noise_pol=0"
write(12,*) "bispectrum_fisher_fwhm_arcmin=7"
write(12,*) "bispectrum_full_output_file="
write(12,*) "bispectrum_full_output_sparse=F"
write(12,*) "bispectrum_export_alpha_beta=F"
write(12,*) "feedback_level = 0"
write(12,*) "output_file_headers = T"
write(12,*) "derived_parameters = T"
write(12,*) "lensing_method = 1"
write(12,*) "accurate_BB = F"
write(12,*) "massive_nu_approx = 1"
write(12,*) "accurate_polarization   = T"
write(12,*) "accurate_reionization   = T"
write(12,*) "do_tensor_neutrinos     = T"
write(12,*) "accurate_massive_neutrino_transfers = F"
write(12,*) "do_late_rad_truncation   = T"
write(12,*) "halofit_version="
write(12,*) "number_of_threads       = 0"
write(12,*) "high_accuracy_default=T"
write(12,*) "accuracy_boost          = 1"
write(12,*) "l_accuracy_boost        = 1"
write(12,*) "l_sample_boost          = 1"

close(12)

call execute_command_line ("./camb search-random-camb-params.ini")

open (unit = 12, file = 'thetastar.txt', status = 'old')
read(12,*) thetastar
close(12)

! chi^2=(theta_*-theta_*,LCDM)^2/(sigma_theta^2)
chi2 = (thetastar - thetastarLCDM)**2/(sigmathetastarLCDM**2)

write(11,"(11e25.16)") ombh2,omch2,hubble,myparameter1,myparameter2,myparameter3,myparameter4,logA,ns,thetastar,chi2

if (chi2 < chi2result) then
ombh2result = ombh2
omch2result = omch2
hubbleresult = hubble
myparameter1result = myparameter1
myparameter2result = myparameter2
myparameter3result = myparameter3
myparameter4result = myparameter4
logAresult = logA
nsresult = ns
thetastarresult = thetastar
chi2result = chi2
ntry = 0
else
ntry = ntry + 1
end if

end do

close(11)

write(*,*) "ombh2result = ",ombh2result
write(*,*) "omch2result = ",omch2result
write(*,*) "hubbleresult = ",hubbleresult
write(*,*) "myparameter1result = ",myparameter1result
write(*,*) "myparameter2result = ",myparameter2result
write(*,*) "myparameter3result = ",myparameter3result
write(*,*) "myparameter4result = ",myparameter4result
write(*,*) "logAresult = ",logAresult
write(*,*) "nsresult = ",nsresult
write(*,*) "thetastarresult = ",thetastarresult
write(*,*) "chi2result = ",chi2result

end program search
