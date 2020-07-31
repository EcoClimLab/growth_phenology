//
data{
    int N;
    real DOY[N];
    real wood_type[N];
    real marchmean[N];
    int sp[N];
    real wood_type_X_marchmean[N];
    int N_sp;
}

parameters{
    real Intercept;
    real beta_wood_type;
    real beta_marchmean;
    real beta_wood_type_X_marchmean;
    real<lower=0> sigma;
    real vary_sp[N_sp];
    real<lower=0> sigma_sp;
}

model{
    real vary[N];
    real glm[N];
    // Priors
    Intercept ~ normal( 0 , 100 );
    beta_wood_type ~ normal( 0 , 100 );
    beta_marchmean ~ normal( 0 , 100 );
    beta_wood_type_X_marchmean ~ normal( 0 , 100 );
    sigma_sp ~ uniform( 0 , 100 );
    sigma ~ uniform( 0 , 100 );
    // Varying effects
    for ( j in 1:N_sp ) vary_sp[j] ~ normal( 0 , sigma_sp );
    // Fixed effects
    for ( i in 1:N ) {
        vary[i] <- vary_sp[sp[i]];
        glm[i] <- vary[i] + Intercept
                + beta_wood_type * wood_type[i]
                + beta_marchmean * marchmean[i]
                + beta_wood_type_X_marchmean * wood_type_X_marchmean[i];
    }
    DOY ~ normal( glm , sigma );
}

generated quantities{
    real dev;
    real vary[N];
    real glm[N];
    dev <- 0;
    for ( i in 1:N ) {
        vary[i] <- vary_sp[sp[i]];
        glm[i] <- vary[i] + Intercept
                + beta_wood_type * wood_type[i]
                + beta_marchmean * marchmean[i]
                + beta_wood_type_X_marchmean * wood_type_X_marchmean[i];
        dev <- dev + (-2) * normal_log( DOY[i] , glm[i] , sigma );
    }
}
