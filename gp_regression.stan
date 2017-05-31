functions{

	# GP: computes noiseless Gaussian Process given pre-computed unique distances
	vector GP(real volatility, real amplitude, vector normal01, int n_x, int n_dx, vector dx_unique, int [,] dx_index) {

		# covars: unique entries in covariance matrix
		vector[n_dx] covars ;

		# covMat: covariance matrix
		matrix[n_x,n_x] covMat ;

		# compute unique entries in covariance matrix
		covars = exp(dx_unique*(volatility^2))*(amplitude^2) ;
		covars[1] = (amplitude^2) + .001 ; #jitter diagonal for positive definiteness

		# loop throug the covariance matrix, copying in entries
		for(i in 1:(n_x-1)){
			covMat[i,i] = covars[1] ;
			for(j in (i+1):n_x){
				covMat[i,j] = covars[dx_index[i,j]] ;
				covMat[j,i] = covMat[i,j] ;
			}
		}
		covMat[n_x,n_x] = covars[1] ; #final diagonal entry

		# combine with normal01 & return
		return(cholesky_decompose(covMat) * normal01 ) ;

	}

}

data {

	# n_dx: number of unique x-distances
	int<lower=1> n_dx ;

	# dx_unique: vector of unique x-distances
	vector[n_dx] dx_unique ;

	# n_x: number of unique x values
	int<lower=1> n_x ;

	# dx_index: matrix of indices into the x-distance matrix of each value in dx_unique
	int dx_index[n_x,n_x] ;

	# n_y: number of observations in y
	int<lower=1> n_y ;

	# y: vector of observations for y
	vector[n_y] y ;

	# x_index: vector indicating which x is associated with each y
	int x_index[n_y] ;

	# n_w: number of columns in predictor matrix w
	int n_w ;

	# w: predictor matrix (each column gets its own GP)
	matrix[n_y,n_w] w ;

	# n_subj: number of subjects
	int<lower=1> n_subj ;

	# n_subj: vector indicating which subject is associated with each y
	int<lower=1,upper=n_subj> subj[n_y] ;

}

transformed data{

	# y_scaled: z-transform of y (for easy prior specification)
	vector[n_y] y_scaled ;
	y_scaled = (y-mean(y))/sd(y) ;

}

parameters {

	# subj_noise: noise per subject
	vector[n_subj] subj_noise ;

	# subj_noise_sd: sd of subj_noise values
	real<lower=0> subj_noise_sd ;

	# volatility_helper: helper for cauchy-distributed volatility (see transformed parameters)
	vector<lower=0,upper=pi()/2>[n_w] volatility_helper ;

	# subj_volatility_helper: helper for cauchy-distributed volitilities per subject (see transformed parameters)
	vector<lower=0,upper=pi()/2>[n_subj] subj_volatility_helper[n_w] ;

	# subj_volatility_sd: sd of subject volitilities
	vector<lower=0>[n_w] subj_volatility_sd ;

	# amplitude: amplitude of population GPs
	vector<lower=0>[n_w] amplitude ;

	# subj_amplitude: amplitude of per-subject GPs
	vector<lower=0>[n_subj] subj_amplitude[n_w] ;

	# subj_amplitude_sd: sd of subj_amplitude
	vector<lower=0>[n_w] subj_amplitude_sd ;

	# f_normal01: helper variable for population GPs (see transformed parameters)
	vector[n_x] f_normal01[n_w] ;

	# f_normal01: helper variable for per-subject GPs (see transformed parameters)
	vector[n_x] subj_f_normal01[n_w,n_subj] ;

}

transformed parameters{

	# volatility: volatility of population GPs
	vector[n_w] volatility ;

	# volatility: volatility of per-subject GPs
	vector[n_subj] subj_volatility[n_w] ;

	# f: population GPs
	vector[n_x] f[n_w] ;

	# subj_f: per-subject GPs
	vector[n_x] subj_f[n_w,n_subj] ;

	#next line implies volatility ~ cauchy(0,10)
	volatility = 10*tan(volatility_helper) ;

	# loop over predictors, computing population GP and per-subject GPs
	for(wi in 1:n_w){

		# next line implies subj_volatility ~ cauchy(0,subj_volatility_sd)
		subj_volatility[wi] = subj_volatility_sd[wi] * tan(subj_volatility_helper[wi]) ;

		# population GP
		f[wi] = GP(
			volatility[wi]
			, amplitude[wi]
			, f_normal01[wi]
			, n_x , n_dx , dx_unique , dx_index
		) ;

		# loop over subjects, computing per-subject GPs
		for(si in 1:n_subj){

			# per-subject GP
			subj_f[wi,si] = f[wi] +
				GP(
					subj_volatility[wi][si]
					, subj_amplitude[wi][si]
					, subj_f_normal01[wi,si]
					, n_x , n_dx , dx_unique , dx_index
				) ;

		}

	}

}

model {

	# noise priors
	subj_noise_sd ~ weibull(2,1) ; #peaked at .8ish
	subj_noise ~ normal(0,subj_noise_sd) ;  #peaked at 0

	# volatility priors:
	# - population GPs have volatility ~ cauchy(0,10)
	# - per-subject GPs have subj_volatility ~ cauchy(0,subj_volatility_sd)
	# - subj_volatility pooled via subj_volatility_sd
	subj_volatility_sd ~ weibull(2,10) ;#peaked around 8

	# amplitude priors
	# - population GPs have amplitude as weibull peaked at .8
	# - per-subject GPs have amplitude as normal peaked at zero with pooled sd
	amplitude ~ weibull(2,1) ; #peaked at .8ish
	subj_amplitude_sd ~ weibull(2,1) ;#peaked at .8ish
	for(wi in 1:n_w){
		subj_amplitude[wi] ~ normal(0,subj_amplitude_sd[wi]) ; #peaked at 0
	}

	# normal(0,1) priors on GP helpers
	for(wi in 1:n_w){
		f_normal01[wi] ~ normal(0,1);
		for(si in 1:n_subj){
			subj_f_normal01[wi,si] ~ normal(0,1) ;
		}
	}

	# set local environment to so subj_noise_exp isn't saved
	{
		# subj_noise_exp: exponentiated subj_noise
		vector[n_subj] subj_noise_exp ;
		subj_noise_exp = exp(subj_noise) ;

		# loop over observations
		for(yi in 1:n_y){
			real temp ;
			temp = 0 ;
			# loop over predictors to gather sum for this observation's mean
			for(wi in 1:n_w){
				temp = temp + w[yi,wi] * subj_f[wi,subj[yi]][x_index[yi]] ;
			}
			# y_scaled as temp with gaussian noise
			y_scaled[yi] ~ normal( temp , subj_noise_exp[subj[yi]] ) ;
		}
	}

}
