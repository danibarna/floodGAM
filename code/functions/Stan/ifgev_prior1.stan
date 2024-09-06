
functions {
  real IF_gev_lpdf(vector y, real mu, real sigma, real xi){
      int N = rows(y);
      vector[N] lpdf;
      vector[N] b;
      vector[N] a;
      
      // ------ likelihood --------
      a = (y - mu) / sigma;
      b = 1 + a * xi;
      if(fabs(xi) > 1e-15){ //if xi =/= 0
      for(i in 1:N)
        lpdf[i] = -log(sigma) - (1 + 1/xi)*log(b[i]) - pow(b[i],(-1/xi));
      } else{
      for(i in 1:N)
        lpdf[i] = -log(sigma) - a[i] - exp(-a[i]);
      }
      
      // check support
      if (sigma<=0)
        print("sigma<=0; found sigma =", sigma);
      if(xi*min((y-mu)/sigma) <= -1){
        print("outside support");
        print("found xi = ", xi);
        print("found mu = ", mu);
        print("found sigma = ", sigma);
        print("for lpdf = ", lpdf);
    }
  return sum(lpdf);
 }
}

data {
  int<lower=0> N;
  vector[N] y;
}

transformed data {
  real y_min = min(y);
  real y_max = max(y);
  real L = log(log(2));
}

parameters {
  real<lower = y_min, upper = y_max> qind;
  real<upper = log((y_min-qind)/(qind*exp(1)*L))> beta;
  real<lower=lambert_w0((qind/(qind-y_max))*exp(beta)*L)/L,
       upper=lambert_w0((qind/(qind-y_min))*exp(beta)*L)/L> xi; 
}

transformed parameters {
 real sigma = qind * exp(beta);
 real mu;
  if(fabs(xi) > 1e-15){
    mu = qind - sigma * ( pow(log(2),-xi) - 1 )/xi;
  } else{
    mu = qind + sigma * log(log(2));
  }
}

model {
  target += normal_lpdf(qind | 400, 1000);
  target += normal_lpdf(beta | 0, 100);
  target += beta_lpdf(-xi + 0.5 | 6, 9);
  target += IF_gev_lpdf(y | mu, sigma, xi);
}
