#problema B1

B1=function(n,p)
{
  variabile_aleatoare<-rgeom(n, p);
  media<-mean(variabile_aleatoare);
  deviatia_standard<-sd(variabile_aleatoare);
  eroarea_standard_medie<-deviatia_standard/sqrt(n);
  eroarea_medie<-abs(media-(1/p));
  
  cat("n=", n, " p=", p, "\n");
  cat("Media=", media, "\n");
  cat("Valoarea=", 1/p, "\n");
  cat("Eroarea medie=", eroarea_medie, "\n");
  cat("Eroarea standard a mediei=", eroarea_standard_medie, "\n");
}

n<-c(5000, 10000, 100000, 500000);
p<-c(0.2, 0.6, 0.6, 0.8);

#problema B2

B2=function(n,N,r,z)
{
  variabile_aleatoare<-matrix(rt(n*N, r), ncol=N);
  medii<-rowMeans(variabile_aleatoare);
  deviatia_standard<-sqrt(r/(r-2));
  eroarea_absoluta<-abs(medii-0);
  
  cat("Eroarea absoluta=", eroarea_absoluta[z+2], "\n");
}

n<-50;
N<-c(5000, 10000, 20000);
r<-n-2;
z<-c(-1.5, 0, 1.5);

#problema B3

B3=function(n,p,h,k)
{
  mu<-n*p;
  sigma<-sqrt(n*p*(1-p));
  ph<-ifself(h==0, 0, pnorm(h, mu, sigma));
  pk<-ifself(k==n, 1, pnorm(k, mu, sigma));
  aproximare<-pk-ph;
  return (aproximare);
}