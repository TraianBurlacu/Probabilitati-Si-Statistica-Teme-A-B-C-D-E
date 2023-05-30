#problema C1

# Functia pentru a verifica daca un punct se afla in paraboloidul de revolutie
in_paraboloid <- function(x1, x2, x3, a) {
  x3 >= x1^2 / (1 + x2^2) & x3 <= a;
}

# Functia pentru a estima volumul paraboloidului de revolutie utilizand metoda Monte Carlo
C1 <- function(a, n) {
  contor <- 0;
  for (i in 1:n) {
    # Genereaza un punct aleator in intervalul dat
    x1 <- runif(1, -sqrt(a), sqrt(a));
    x2 <- runif(1, -sqrt(a), sqrt(a));
    x3 <- runif(1, 0, a);
    
    # Verifica daca punctul se afla in paraboloid
    if (in_paraboloid(x1, x2, x3, a)) {
      contor <- contor + 1;
    }
  }
  
  # Calculeaza volumul estimat
  volum_estimat <- (contor / n) * (4 * a^2);
  
  # Returneaza volumul estimat
  return(volum_estimat);
}

# Seteaza seed-ul pentru reproducibilitate
set.seed(42)

# Parametrii paraboloidului
a_valori <- c(2, 4, 10);

# Dimensiuni ale esantioanelor
dim_esantioane <- c(10000, 20000, 50000);

# Calculeaza volumul estimat si erorile relative
for (a in a_valori) {
  for (dim in dim_esantioane) {
    # Estimeaza volumul
    volum_estimat <- C1(a, dim);
    
    # Calculeaza eroarea relativa
    eroare_relativa <- abs(volum_estimat - (pi * a^2 / 2)) / (pi * a^2 / 2)
    
    cat(paste("Pentru a =", a, "si dimensiunea esantionului =", dim, "volumul estimat este:", volum_estimat, "\n"))
    cat(paste("Eroarea relativa este:", eroare_relativa, "\n\n"))
  }
}

#problema C2

# Functia pentru a verifica daca un punct se afla in patrulater
in_patrulater <- function(x, y) {
  x >= 0 & y >= 0 & 3*y <= x + 6 & y <= 12 - 3*x;
}

# Determina zona rectangulara care include toate punctele interioare ale patrulaterului
a <- 0
b <- 6
c <- 0
d <- 4

# Functia pentru a estima aria patrulaterului utilizand metoda Monte Carlo
C2 <- function(n) {
  contor <- 0;
  for (i in 1:n) {
    # Genereaza un punct aleator in zona rectangulara
    x <- runif(1, a, b);
    y <- runif(1, c, d);
    
    # Verifica daca punctul se afla in patrulater
    if (in_patrulater(x, y)) {
      contor <- contor + 1;
    }
  }
  
  # Calculeaza aria estimata
  aria_estimata <- (contor / n) * ((b - a) * (d - c));
  
  # Returneaza aria estimata
  return(aria_estimata);
}

# Seteaza seed-ul pentru reproducibilitate
set.seed(42);

# Dimensiunea esantionului
dim_esantion <- 20000;

# Estimeaza aria
aria_estimata <- C2(dim_esantion);

# Afiseaza rezultatele
cat(paste("Aria estimata a patrulaterului T este:", aria_estimata, "\n"));

#problema C3

#a)

# Functia pentru a estima valoarea integralei utilizand metoda Monte Carlo
C3a <- function(n) {
  suma <- 0;
  for (i in 1:n) {
    # Genereaza un punct aleator in intervalul dat
    x <- runif(1, -1, 1);
    
    # Calculeaza valoarea functiei in punctul generat
    f_x <- (x + 1) / sqrt(4 - x^2);
    
    # Adauga la suma
    suma <- suma + f_x;
  }
  
  # Calculeaza valoarea estimata a integralei
  valoare_estimata <- suma * 2 / n;
  
  # Returneaza valoarea estimata
  return(valoare_estimata);
}

# Seteaza seed-ul pentru reproducibilitate
set.seed(42)

# Dimensiunea esantionului
dim_esantion <- 20000;

# Estimeaza valoarea integralei
valoare_estimata <- C3a(dim_esantion);

# Valoarea exacta a integralei
valoare_exacta <- pi / 3;

# Calculeaza eroarea relativa
eroare_relativa <- abs(valoare_estimata - valoare_exacta) / valoare_exacta;

# Afiseaza rezultatele
cat(paste("Valoarea estimata a integralei este:", valoare_estimata, "\n"));
cat(paste("Valoarea exacta a integralei este:", valoare_exacta, "\n"));
cat(paste("Eroarea relativa este:", eroare_relativa, "\n"));

#b)

# Functia pentru a estima valoarea primei integrale utilizand metoda Monte Carlo
estimeaza_integrala_1 <- function(n) {
  suma <- 0
  for (i in 1:n) {
    # Genereaza un punct aleator in intervalul dat
    x <- runif(1)
    
    # Calculeaza valoarea functiei in punctul generat
    f_x <- 1 / (x^2 + 4)
    
    # Adauga la suma
    suma <- suma + f_x
  }
  
  # Calculeaza valoarea estimata a primei integrale
  valoare_estimata <- suma / n
  
  # Returneaza valoarea estimata
  return(valoare_estimata)
}

# Functia pentru a estima valoarea celei de-a doua integrale utilizand metoda Monte Carlo
estimeaza_integrala_2 <- function(n, a) {
  suma <- 0;
  for (i in 1:n) {
    # Genereaza un punct aleator in intervalul dat
    x <- runif(1, 0, a);
    
    # Calculeaza valoarea functiei in punctul generat
    f_x <- 1 / (x^2 + 4);
    
    # Adauga la suma
    suma <- suma + f_x;
  }
  
  # Calculeaza valoarea estimata a celei de-a doua integrale
  valoare_estimata <- suma * (a - 0) / n;
  
  # Returneaza valoarea estimata
  return(valoare_estimata);
}

# Seteaza seed-ul pentru reproducibilitate
set.seed(42);

# Dimensiunea esantionului
dim_esantion <- 20000;

# Estimeaza valoarea primei integrale
valoare_estimata_1 <- estimeaza_integrala_1(dim_esantion);

# Valoarea exacta a primei integrale
valoare_exacta_1 <- pi / 4;

# Calculeaza eroarea relativa pentru prima integrala
eroare_relativa_1 <- abs(valoare_estimata_1 - valoare_exacta_1) / valoare_exacta_1;

# Aproximeaza limita
limita_aproximata <- estimeaza_integrala_2(dim_esantion, -1e6);

# Valoarea exacta a celei de-a doua integrale
valoare_exacta_2 <- pi / 4;

# Calculeaza eroarea relativa pentru a doua integrala
eroare_relativa_2 <- abs(limita_aproximata - valoare_exacta_2) / valoare_exacta_2;

# Afiseaza rezultatele
cat(paste("Valoarea estimata a primei integrale este:", valoare_estimata_1, "\n"));
cat(paste("Valoarea exacta a primei integrale este:", valoare_exacta_1, "\n"));
cat(paste("Eroarea relativa pentru prima integrala este:", eroare_relativa_1, "\n\n"));
cat(paste("Limita aproximata a celei de-a doua integrale este:", limita_aproximata, "\n"));
cat(paste("Valoarea exacta a celei de-a doua integrale este:", valoare_exacta_2, "\n"));
cat(paste("Eroarea relativa pentru a doua integrala este:", eroare_relativa_2, "\n"));

#c)

# Functia pentru a estima valoarea primei integrale utilizand metoda Monte Carlo
estimeaza_integrala_1 <- function(n) {
  suma <- 0
  for (i in 1:n) {
    # Genereaza un punct aleator in intervalul dat
    x <- runif(1)
    
    # Calculeaza valoarea functiei in punctul generat
    f_x <- x * exp(x)
    
    # Adauga la suma
    suma <- suma + f_x
  }
  
  # Calculeaza valoarea estimata a primei integrale
  valoare_estimata <- suma / n
  
  # Returneaza valoarea estimata
  return(valoare_estimata)
}

# Functia pentru a estima valoarea celei de-a doua integrale utilizand metoda Monte Carlo
estimeaza_integrala_2 <- function(n, a) {
  suma <- 0;
  for (i in 1:n) {
    # Genereaza un punct aleator in intervalul dat
    x <- runif(1, 0, a);
    
    # Calculeaza valoarea functiei in punctul generat
    f_x <- x * exp(x);
    
    # Adauga la suma
    suma <- suma + f_x;
  }
  
  # Calculeaza valoarea estimata a celei de-a doua integrale
  valoare_estimata <- suma * (a - 0) / n;
  
  # Returneaza valoarea estimata
  return(valoare_estimata);
}

# Seteaza seed-ul pentru reproducibilitate
set.seed(42);

# Dimensiunea esantionului
dim_esantion <- 20000;

# Estimeaza valoarea primei integrale
valoare_estimata_1 <- estimeaza_integrala_1(dim_esantion);

# Valoarea exacta a primei integrale
valoare_exacta_1 <- -1;

# Calculeaza eroarea relativa pentru prima integrala
eroare_relativa_1 <- abs(valoare_estimata_1 - valoare_exacta_1) / abs(valoare_exacta_1);

# Aproximeaza limita
limita_aproximata <- estimeaza_integrala_2(dim_esantion, -1e6);

# Valoarea exacta a celei de-a doua integrale
valoare_exacta_2 <- -1;

# Calculeaza eroarea relativa pentru a doua integrala
eroare_relativa_2 <- abs(limita_aproximata - valoare_exacta_2) / abs(valoare_exacta_2);

# Afiseaza rezultatele
cat(paste("Valoarea estimata a primei integrale este:", valoare_estimata_1, "\n"));
cat(paste("Valoarea exacta a primei integrale este:", valoare_exacta_1, "\n"));
cat(paste("Eroarea relativa pentru prima integrala este:", eroare_relativa_1, "\n"));
cat(paste("Limita aproximata a celei de-a doua integrale este:", limita_aproximata, "\n"));
cat(paste("Valoarea exacta a celei de-a doua integrale este:", valoare_exacta_2, "\n"));
cat(paste("Eroarea relativa pentru a doua integrala este:", eroare_relativa_2, "\n"));

#problema C4

#a)

C4a <- function(m, n, p, q) {
  numar_zile <- 0;
  conturi_false <- m;
  
  while (conturi_false > 0) {
    # Adauga numarul de conturi false nou adaugate in ziua curenta
    conturi_false <- conturi_false + rbinom(1, n, p);
    
    # Dezactiveaza conturile false cu probabilitate q
    dezactivate <- rbinom(conturi_false, 1, q);
    
    # Actualizeaza numarul de conturi false
    conturi_false <- sum(dezactivate);
    
    # Incrementeaza numarul de zile
    numar_zile <- numar_zile + 1;
  }
  
  # Returneaza numarul mediu de zile
  return(numar_zile);
}

# Seteaza seed-ul pentru reproducibilitate
set.seed(42);

# Parametrii
m <- 100000   # Numar initial de conturi false
n <- 500    # Numarul de conturi false adaugate in fiecare zi (parametrul binomial)
p <- 0.5    # Probabilitatea de adaugare a unui cont fals in fiecare zi (parametrul binomial)
q <- 0.1    # Probabilitatea de dezactivare a unui cont fals (probabilitatea IA)

# Dimensiunea esantionului
dim_esantion <- 1000;

# Estimeaza numarul mediu de zile
rezultate <- replicate(dim_esantion, C4a(m, n, p, q));

# Calculeaza valoarea estimata a numarului mediu de zile
numar_mediu_zile <- mean(rezultate);

# Afiseaza rezultatul
cat(paste("Numarul mediu de zile pana cand devine noul proprietar al SocialNetworkOne este:", numar_mediu_zile, "\n"));

#b)

# Functia pentru a estima probabilitatea
C4b <- function(m, n, p, q, zile, prag) {
  conturi_false <- m;
  
  for (i in 1:zile) {
    # Adauga numarul de conturi false nou adaugate in ziua curenta
    conturi_false <- conturi_false + rbinom(1, n, p);
    
    # Dezactiveaza conturile false cu probabilitate q
    dezactivate <- rbinom(conturi_false, 1, q);
    
    # Actualizeaza numarul de conturi false
    conturi_false <- sum(dezactivate);
    
    # Verifica pragul
    if (conturi_false <= prag) {
      return(TRUE);
    }
  }
  
  return(FALSE);
}

# Seteaza seed-ul pentru reproducibilitate
set.seed(42);

# Parametrii
m <- 100000    # Numar initial de conturi false
n <- 500     # Numarul de conturi false adaugate in fiecare zi (parametrul binomial)
p <- 0.5     # Probabilitatea de adaugare a unui cont fals in fiecare zi (parametrul binomial)
q <- 0.1     # Probabilitatea de dezactivare a unui cont fals (probabilitatea IA)
zile <- 40   # Numarul de zile
prag <- 50000  # Pragul de conturi false

# Dimensiunea esantionului
dim_esantion <- 1000;

# Estimeaza probabilitatea
rezultate <- replicate(dim_esantion, C4b(m, n, p, q, zile, prag));

# Calculeaza probabilitatea estimata
probabilitate_estimata <- mean(rezultate);

# Afiseaza rezultatul
cat(paste("Probabilitatea ca dupa", zile, "zile sa mai existe cel mult", prag, "conturi false este:", probabilitate_estimata, "\n"));

#c)

# Functia pentru a estima probabilitatea cu eroare si nivel de incredere specificate
C4c <- function(m, n, p, q, dim_esantion, eroare, nivel_incredere) {
  rezultate <- numeric(dim_esantion);
  
  for (i in 1:dim_esantion) {
    conturi_false <- m;
    zile <- ;
    
    while (TRUE) {
      # Adauga numarul de conturi false nou adaugate in ziua curenta
      conturi_false <- conturi_false + rbinom(1, n, p);
      
      # Dezactiveaza conturile false cu probabilitate q
      dezactivate <- rbinom(conturi_false, 1, q);
      
      # Actualizeaza numarul de conturi false
      conturi_false <- sum(dezactivate);
      
      # Verifica daca probabilitatea estimata este suficient de precisa
      probabilitate_estimata <- rezultate[i] / zile;
      eroare_estimata <- 1.96 * sqrt(probabilitate_estimata * (1 - probabilitate_estimata) / zile);
      if (eroare_estimata <= eroare) {
        break
      }
      
      # Incrementeaza numarul de zile
      zile <- zile + 1;
    }
    
    rezultate[i] <- zile;
  }
  
  # Calculeaza intervalul de incredere
  probabilitate_estimata <- dim_esantion / sum(rezultate);
  eroare_estimata <- 1.96 * sqrt(probabilitate_estimata * (1 - probabilitate_estimata) / sum(rezultate));
  interval_incredere <- c(probabilitate_estimata - eroare_estimata, probabilitate_estimata + eroare_estimata);
  
  return(interval_incredere);
}

# Seteaza seed-ul pentru reproducibilitate
set.seed(42)

# Parametrii
m <- 100000    # Numar initial de conturi false
n <- 500     # Numarul de conturi false adaugate in fiecare zi (parametrul binomial)
p <- 0.5     # Probabilitatea de adaugare a unui cont fals in fiecare zi (parametrul binomial)
q <- 0.1     # Probabilitatea de dezactivare a unui cont fals (probabilitatea IA)
dim_esantion <- 1000    # Dimensiunea esantionului
eroare <- 0.01          # Eroarea maximala permisa
nivel_incredere <- 0.99  # Nivelul de incredere

# Estimeaza probabilitatea cu interval de incredere
interval <- C4c(m, n, p, q, dim_esantion, eroare, nivel_incredere);

# Afiseaza rezultatul
cat(paste("Probabilitatea estimata cu o eroare de cel mult ±", eroare, "si un nivel de incredere de", nivel_incredere, "este:", interval[1], "-", interval[2], "\n"));

#d)


