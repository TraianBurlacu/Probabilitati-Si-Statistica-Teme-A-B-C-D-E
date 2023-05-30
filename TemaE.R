#problema E1

E1=function(media, deviatia_standard, numar_masuratori, nivel_confidenta)
{
  media <- 138;
  deviatia_standard <- 11;
  numar_masuratori <- 10;
  
  nivel_confidenta <- c(0.9, 0.95, 0.99);
  
  intervale_incredere <- sapply(nivel_confidenta, function(confidenta) {
    cuantile <- qnorm(c((1 - confidenta) / 2, 1 - (1 - confidenta) / 2));
    marja_eroare <- cuantile * deviatia_standard / sqrt(numar_masuratori);
    interval <- c(media - marja_eroare, media + marja_eroare);
    
  })
  
  for (i in 1:length(nivel_confidenta)) {
    cat("Interval de incredere", nivel_confidenta[i] * 100, "%: [",
        round(intervale_incredere[1, i], 4), ",", round(intervale_incredere[2, i], 4), "]\n");
  }
  
}

#problema E2

E2 <- function(media_selectie, dispersie_aleatoare, dimensiune_aleatoare) {
  nivel_confidenta <- 0.95;
  media_selectie <- 18;
  dispersie_aleatoare <- 1.44;
  dimensiune_aleatoare <- 256;
  
  # Calculăm cuantila corespunzătoare nivelului de confidență
  cuantila <- qnorm((1 + nivel_confidenta) / 2);
  
  # Calculăm marginea superioară a intervalului de încredere
  margine_superioara <- media_selectie + cuantila * sqrt(dispersie_aleatoare / dimensiune_aleatoare);
  
  # Calculăm marginea inferioară a intervalului de încredere
  margine_inferioara <- media_selectie - cuantila * sqrt(dispersie_aleatoare / dimensiune_aleatoare);
  
  # Returnăm intervalul de încredere
  interval <- c(margine_inferioara, margine_superioara);
  return(interval);
}

# Calculăm intervalul de încredere de 95%
interval_95 <- E2(media_selectie, dispersie_aleatoare, dimensiune_aleatoare);

# Afișăm rezultatul
cat("Interval de incredere de 95%: [",
    round(interval_95[1], 4), ",", round(interval_95[2], 4), "]\n");

#problema E3

E3 <- function(nemultumiti_anteschimbare, dimensiune_aleatoare, nivel_semnificatie) {
  nemultumiti_dupaschimbare <- sum(sample(c(0, 1), size = dimensiune_aleatoare, replace = TRUE, prob = c(0.88, 0.12)));
  
  # Calculăm proporția nemulțumiților după schimbare
  proportie_dupaschimbare <- nemultumiti_dupaschimbare / dimensiune_aleatoare;
  
  # Calculăm proporția nemulțumiților înainte de schimbare
  proportie_anteschimbare <- nemultumiti_anteschimbare / dimensiune_aleatoare;
  
  # Calculăm eroarea standard pentru diferența proporțiilor
  eroare_standard <- sqrt(proportie_dupaschimbare * (1 - proportie_dupaschimbare) / dimensiune_aleatoare +
                            proportie_anteschimbare * (1 - proportie_anteschimbare) / dimensiune_aleatoare);
  
  # Calculăm diferența proporțiilor și testăm ipoteza nulă
  diferenta_proportii <- proportie_anteschimbare - proportie_dupaschimbare;
  z_score <- diferenta_proportii / eroare_standard;
  
  # Calculăm cuantila corespunzătoare nivelului de semnificație
  cuantila <- qnorm(1 - nivel_semnificatie);
  
  # Verificăm dacă z_score depășește cuantila
  if (z_score > cuantila) {
    concluzie <- "Schimbarea a fost inutilă (ipoteza nulă respinsă)";
  } else {
    concluzie <- "Schimbarea a fost semnificativă (ipoteza nulă nu poate fi respinsă)";
  }
  
  return(concluzie);
}

# Apelăm funcția cu valorile 
nemultumiti_anteschimbare <- 0.12 * 153
dimensiune_aleatoare <- 153
nivel_semnificatie_1 <- 0.01
nivel_semnificatie_5 <- 0.05

# Testăm ipoteza la nivelul de semnificație de 1%
rezultat_1 <- E3(nemultumiti_anteschimbare, dimensiune_aleatoare, nivel_semnificatie_1);

# Testăm ipoteza la nivelul de semnificație de 5%
rezultat_5 <- E3(nemultumiti_anteschimbare, dimensiune_aleatoare, nivel_semnificatie_5);

# Afișăm rezultatele
cat("Rezultat la nivelul de semnificație de 1%: ", rezultat_1, "\n");
cat("Rezultat la nivelul de semnificație de 5%: ", rezultat_5, "\n");
