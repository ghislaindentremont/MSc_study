functions{ # This Stan file is largely cribbed from Mike Lawrence's GP examples

	# GP: computes noiseless Gaussian Process
	vector GP(real volatility, real amplitude, vector normal01, int n_x, real[] x ) {
	  
		matrix[n_x,n_x] cov_mat ;
		real amplitude_sq_plus_jitter ;
		amplitude_sq_plus_jitter = amplitude^2 + 1e-6 ;
		cov_mat = cov_exp_quad(x, amplitude, 1/volatility) ;
		
		for(i in 1:n_x){
			cov_mat[i,i] = amplitude_sq_plus_jitter ;
		}
		
		return(cholesky_decompose(cov_mat) * normal01 ) ;
	}
	
}

data {

	# n_y: number of observations in y
	int n_y ;
	
	# n_subj: number of subjects
	int n_subj ;

	# y: vector of observations for y; should be scaled to mean=0,sd=1
	vector[10000] y[n_subj] ;

	# n_x: number of unique x values
	int n_x ;

	# x: unique values of x; should be scaled to min=0,max=1
	real x[n_x] ;

	# x_index: vector indicating which x is associated zith each y
	int x_index[n_y] ;

	# n_z: number of columns in predictor matrix z
	int n_z ;

	# rows_z_unique: number of rows in predictor matrix z
	int rows_z_unique ;

	# z_unique: predictor matrix (each column gets its own GP)
	matrix[rows_z_unique,n_z] z_unique ;

	# z_by_f_by_s_pad: matrix containing function values for each participant
	int z_by_f_by_s_pad[n_subj, 10000] ;
	
	# vector indicating the length of the GP index vector for each participant
	int subj_obs[n_subj] ;
	
}

parameters {
	
	// NOISE (Trial-wise variability)
	# volatility_helper: helper for cauchy-distributed volatility (see transformed parameters)
	vector<lower=0,upper=pi()/2>[n_z] noise_volatility_helper ;

	# subj_volatility_helper: helper for cauchy-distributed volitilities per subject (see transformed parameters)
	vector<lower=0,upper=pi()/2>[n_subj] noise_subj_volatility_helper[n_z] ;

	# subj_volatility_sd: sd of subject volitilities
	vector<lower=0>[n_z] noise_subj_volatility_sd ;

	# amplitude: amplitude of population GPs
	vector<lower=0>[n_z] noise_amplitude ;

	# subj_amplitude: amplitude of per-subject GPs
	vector<lower=0>[n_subj] noise_subj_amplitude[n_z] ;

	# subj_amplitude_sd: sd of subj_amplitude
	vector<lower=0>[n_z] noise_subj_amplitude_sd ;

	# f_normal01: helper variable for GPs (see transformed parameters)
	matrix[n_x, n_z] noise_f_normal01 ;

	# f_normal01: helper variable for per-subject GPs (see transformed parameters)
	matrix[n_x, n_z] noise_subj_f_normal01[n_subj] ;
	
	
  // DISPLACEMENT (Trajectories)
	# volatility_helper: helper for cauchy-distributed volatility (see transformed parameters)
	vector<lower=0,upper=pi()/2>[n_z] volatility_helper ;

	# subj_volatility_helper: helper for cauchy-distributed volitilities per subject (see transformed parameters)
	vector<lower=0,upper=pi()/2>[n_subj] subj_volatility_helper[n_z] ;

	# subj_volatility_sd: sd of subject volitilities
	vector<lower=0>[n_z] subj_volatility_sd ;

	# amplitude: amplitude of population GPs
	vector<lower=0>[n_z] amplitude ;

	# subj_amplitude: amplitude of per-subject GPs
	vector<lower=0>[n_subj] subj_amplitude[n_z] ;

	# subj_amplitude_sd: sd of subj_amplitude
	vector<lower=0>[n_z] subj_amplitude_sd ;

	# f_normal01: helper variable for GPs (see transformed parameters)
	matrix[n_x, n_z] f_normal01 ;

	# f_normal01: helper variable for per-subject GPs (see transformed parameters)
	matrix[n_x, n_z] subj_f_normal01[n_subj] ;

}

transformed parameters{

  // NOISE (Trial-wise variability)
  # volatility: volatility of population GPs
	vector[n_z] noise_volatility ;

	# volatility: volatility of per-subject GPs
	vector[n_subj] noise_subj_volatility[n_z] ;

	# f: Population GPs
	matrix[n_x,n_z] noise_f ;

	# subj_f: per-subject GPs
	matrix[n_x,n_z] noise_subj_f[n_subj] ;
	
	
	// DISPLACEMENT (Trajectories)
	# volatility: volatility of population GPs
	vector[n_z] volatility ;

	# volatility: volatility of per-subject GPs
	vector[n_subj] subj_volatility[n_z] ;

	# f: Population GPs
	matrix[n_x,n_z] f ;

	# subj_f: per-subject GPs
	matrix[n_x,n_z] subj_f[n_subj] ;
	
	#next line implies volatility ~ cauchy(0,10)
	volatility = 10*tan(volatility_helper) ;

	#next line implies volatility ~ cauchy(0,10)
	noise_volatility = 10*tan(noise_volatility_helper) ;

	# loop over predictors, computing population GP and per-subject GPs
	for(zi in 1:n_z){

		# next line implies subj_volatility ~ cauchy(0,subj_volatility_sd)
		noise_subj_volatility[zi] = noise_subj_volatility_sd[zi] * tan(noise_subj_volatility_helper[zi]) ;

		# population GP
		noise_f[,zi] = GP(
			  noise_volatility[zi]
			, noise_amplitude[zi]
			, noise_f_normal01[,zi]
			, n_x , x
		) ;

		# loop over subjects, computing per-subject GPs
		for(si in 1:n_subj){

			# per-subject GP
			noise_subj_f[si, ,zi] = noise_f[,zi] +
				GP(
					noise_subj_volatility[zi,si]
					, noise_subj_amplitude[zi,si]
					, noise_subj_f_normal01[si,,zi]
					, n_x , x
				) ;

		}
	}

	# loop over predictors, computing population GP and per-subject GPs
	for(zi in 1:n_z){

		# next line implies subj_volatility ~ cauchy(0,subj_volatility_sd)
		subj_volatility[zi] = subj_volatility_sd[zi] * tan(subj_volatility_helper[zi]) ;

		# population GP
		f[,zi] = GP(
			  volatility[zi]
			, amplitude[zi]
			, f_normal01[,zi]
			, n_x , x
		) ;

		# loop over subjects, computing per-subject GPs
		for(si in 1:n_subj){

			# per-subject GP
			subj_f[si, ,zi] = f[,zi] +
				GP(
					subj_volatility[zi,si]
					, subj_amplitude[zi,si]
					, subj_f_normal01[si,,zi]
					, n_x , x
				) ;

		}
	}

}

model {

	# volatility priors:
	# - population GPs have volatility ~ cauchy(0,10)
	# - per-subject GPs have subj_volatility ~ cauchy(0,subj_volatility_sd)
	# - subj_volatility pooled via subj_volatility_sd
	
	noise_subj_volatility_sd ~ weibull(2,1) ; #peaked around 0.8
	subj_volatility_sd ~ weibull(2,1) ; #peaked around 0.8


	# amplitude priors
	# - population GPs have amplitude as weibull peaked at .8
	# - per-subject GPs have amplitude as normal peaked at zero with pooled sd
	
	noise_amplitude ~ weibull(2,1) ; #peaked at .8ish
	amplitude ~ weibull(2,1) ; #peaked at .8ish
	
	noise_subj_amplitude_sd ~ weibull(2,1) ;#peaked at .8ish
	subj_amplitude_sd ~ weibull(2,1) ;#peaked at .8ish
	
	for(zi in 1:n_z){
		subj_amplitude[zi] ~ normal(0,subj_amplitude_sd[zi]) ; #peaked at 0
		noise_subj_amplitude[zi] ~ normal(0,noise_subj_amplitude_sd[zi]) ; #peaked at 0
	}

	# normal(0,1) priors on GP helpers
	to_vector(f_normal01) ~ normal(0,1);
	to_vector(noise_f_normal01) ~ normal(0,1);
	
	for(si in 1:n_subj){
		to_vector(subj_f_normal01[si]) ~ normal(0,1) ;
		to_vector(noise_subj_f_normal01[si]) ~ normal(0,1) ;
	}
  
  # loop over observations
	{

    # trajectory estimate for each participant
		matrix[rows_z_unique,n_x] z_by_f[n_subj] ;
		
		# noise estimate for each participant 
		matrix[rows_z_unique,n_x] noise_z_by_f[n_subj] ;

		for(i in 1:rows_z_unique){
			for(j in 1:n_x){
			  for(k in 1:n_subj){
				z_by_f[k,i,j] = sum(z_unique[i].*subj_f[k,j,]) ;
				noise_z_by_f[k,i,j] = sum(z_unique[i].*noise_subj_f[k,j,]) ;
			  }
			}
		}
		
		# likelihood
		for(si in 1:n_subj) {
  		y[si,1:subj_obs[si]] ~ normal(
  			  to_vector(z_by_f[si])[z_by_f_by_s_pad[si,1:subj_obs[si]]]
  			, exp(to_vector(noise_z_by_f[si])[z_by_f_by_s_pad[si,1:subj_obs[si]]])
  		);
		}
		
	}

}

