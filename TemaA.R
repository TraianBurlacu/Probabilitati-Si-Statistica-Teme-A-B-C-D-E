A1_a=function(lambda,p,n,k)
{
  for(i in k:n)
  {
    prob<-dpois(i,lambda);
    plot(i, prob, type="h", lwd=10, col="blue", ylim=c(0, max(prob)*1.2), main=".", xlab=".", ylab=".");
    
    prob<-dgeom(i,p);
    plot(i, prob, type="h", lwd=10, col="red", ylim=c(0, max(prob)*1.2), main=".", xlab=".", ylab=".");
    
    prob<-dbinom(x,n,p);
    plot(i, prob, type="h", lwd=10, col="green", ylim=c(0, max(prob)*1.2), main=".", xlab=".", ylab=".");
  }
}

A1_b=funciton(p)
{
  suma1<-0;
  suma2<-0;
  suma3<-0;
  for(i in 1:20)
    if(i%%2==1)
    {
      prob1<-dgeom(i,p);
      suma1<-suma1+prob1;
    }
  
  for(j in 4:20)
  {
    prob2<-dgeom(j,p);
    suma2<-suma2+prob2;
  }
  
  for(k in 1:20)
  {
    prob3<-dgeom(k,p);
    suma3<-suma3+prob3;
  }
  return(suma1, suma2, suma3);
}

A1_c=funciton(lambda)
{
  k<-0;
  prob<-1;
  while(prob>=1e-7)
  {
    prob<-1-ppois(k,lambda);
    k<-k+1;
  }
  return(k-1);
}



A2_a=function(fisier)
{
    df <- readr::read_csv(fisier, col_names = FALSE);
    
    esantion1 <- df[1,];
    esantion2 <- df[2,];
    
    media_esantion1 <- mean(esantion1);
    media_esantion2 <- mean(esantion2);
    
    mediana_esantion1 <- median(esantion1);
    mediana_esantion2 <- median(esantion2);
    
    deviatia_standard_esantion1 <- sd(esantion1);
    deviatia_standard_esantion2 <- sd(esantion2);
    
    cvartila1_esantion1 <- quantile(esantion1, 0.25);
    cvartila2_esantion1 <- quantile(esantion1, 0.50);
    cvartila3_esantion1 <- quantile(esantion1, 0.75);
    
    cvartila1_esantion2 <- quantile(esantion2, 0.25);
    cvartila2_esantion2 <- quantile(esantion2, 0.50);
    cvartila3_esantion2 <- quantile(esantion2, 0.75);
    
    cat("Esantionul 1:\n");
    cat("Media:", media_esantion1, "\n");
    cat("Mediana:", mediana_esantion1, "\n");
    cat("Deviatia standard:", deviatia_standard_esantion1, "\n");
    cat("Q1:", cvartila1_esantion1, "\n");
    cat("Q2:", cvartila2_esantion1, "\n");
    cat("Q3:", cvartila3_esantion1, "\n");
    
    cat("Esantionul 2:\n");
    cat("Media:", media_esantion2, "\n");
    cat("Mediana:", mediana_esantion2, "\n");
    cat("Deviatia standard:", deviatia_standard_esantion2, "\n");
    cat("Q1:", cvartila1_esantion2, "\n");
    cat("Q2:", cvartila2_esantion2, "\n");
    cat("Q3:", cvartila3_esantion2, "\n");
}
A2_a("note.txt");

A2_b=function(fisier, esantion)
{
  df <- readr::read_csv(fisier, col_names=FALSE);
  esantion <- df[df$X1 == P,]$X2;
  boxplot_outliers <- boxplot.stats(esantion)$out;
  esantion_fara_valori_aberante <- esantion[!esantion %in% boxplot_outliers];
  return(esantion_fara_valori_aberante);
}
esantion_P_fara_valori_aberante <- A2_b("note.txt", "P");
esantion_s_fara_valori_aberante <- a2_b("note.txt", "S");



A2_c=function(nume_fisier)
{
  nume_fisier="note.txt";
  df<-readr::read_csv("note.txt", col_names=FALSE);
  esantion_curatat<-df$X2;
  intervale<-seq(1, 10, by=1);
  frecvente<-cut(esantion_curatat, breaks=intervale, right=FALSE);
  frecvente<-table(frecvente);
  barplot(frecvente, main="Distributia frecventelor", xlab="intervale", ylab="frecventa");
}
