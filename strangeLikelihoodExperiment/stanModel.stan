data {
  real priceMean[2]; 
  real priceSD[2];
  real totPriorMean;
  real totPriorSD;
}
parameters {
  real totPrice; 
  real price1;
  real price2;
}

transformed parameters{
  real guessedTotPrice;
  guessedTotPrice = price1 + price2;
}

model {
  target += normal_lpdf(totPrice | totPriorMean, totPriorSD);
  target += normal_lpdf(price1 | priceMean[1], priceSD[1]);
  target += normal_lpdf(price2 | priceMean[2], priceSD[2]);
  target += normal_lpdf( guessedTotPrice | totPrice, 3000 );
}
