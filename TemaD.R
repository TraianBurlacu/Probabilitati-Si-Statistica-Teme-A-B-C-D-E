#problema D1

#a)
D1 <- function(x, k) {
  n <- length(x)
  threshold <- n/2 + 1
  for (i in 1:k) {
    random_index <- sample(1:n, 1)  # Alegere aleatoare a unui index din x
    selected_element <- x[random_index]
    count <- sum(x == selected_element)  # Numărul de apariții ale elementului selectat în x
    if (count >= threshold) {
      return(selected_element)  # Returnăm elementul M
    }
  }
  return("x nu are M-element")  # Dacă nu s-a găsit niciun M-element în cele k iterații
}

x <- c(1, 2, 3, 4, 5, 5, 5, 6, 7, 5)
k <- 1000
result <- D1(x, k)
print(result)

#b)
k <- ceiling(log2(1/(10^-7)))
print(k)

#problema D2

D2 <- function(i, A) {
  n <- length(A)
  
  if (n == 1) {
    return(A)  # Returnăm singurul element din A
  }
  
  z <- sample(A, 1)  # Alegere aleatoare a unui element din A
  
  A_less <- A[A < z]  # Elementele din A mai mici decât z
  A_greater <- A[A > z]  # Elementele din A mai mari decât z
  
  if (length(A_less) >= i) {
    return(D2(i, A_less))  # Apel recursiv pentru elementul i în A_less
  } else if (n > i + length(A_greater)) {
    return(z)  # Elementul z este cel de-al i-lea element din A
  } else {
    return(D2(i - n + length(A_greater), A_greater))  # Apel recursiv pentru elementul i în A_greater
  }
}

A <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
i <- 3
result <- D2(i, A)
print(result)

#problema D3

#a)
D3 <- function(S, a) {
  n <- length(S)
  m <- floor(a * log(n))
  
  if (m >= n) {
    return(median(S))  # Dacă m este mai mare sau egal cu n, returnăm mediana lui S direct
  }
  
  S_prime <- sample(S, m)  # Alegere uniformă la întâmplare a m elemente din S
  sorted_S_prime <- sort(S_prime)  # Sortăm S_prime
  
  return(median(sorted_S_prime))  # Returnăm mediana lui S_prime
}

S <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
a <- 0.5
result <- D3(S, a)
print(result)

#b)
n <- ceiling(sqrt(2/10^-7))
print(n)


