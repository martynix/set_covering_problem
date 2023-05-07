#setwd('C:/Users/marty/Desktop/GITHUB/set_covering_problem')
#setwd('/Users/mateuszpindyk/Documents/GitHub/set_covering_problem')

#dla przykładu ze sprawozdania
k1<-c(1,1,1,0,0,0,0,0,0,0,0)
k2<-c(0,1,1,1,1,0,0,0,0,0,0)
k3<-c(0,1,0,0,1,0,1,0,0,0,0)
k4<-c(0,0,0,0,1,1,1,1,1,0,0)
k5<-c(0,0,0,0,0,0,0,1,1,1,0)
k6<-c(0,0,0,0,0,0,0,1,0,1,1)
k7<-c(0,0,0,1,1,1,0,0,0,0,0)

m<- (data.frame(k1,k2,k3,k4,k5,k6,k7))
l<-c("A","B","C","D","E", "F", "G")
colnames(m)<- l
rownames(m) <- c(1,2,3,4,5,6,7,8,9,10,11)
m

###################
#generowanie losowej macierzy 

n_rows <- sample(1:50, 1)
n_cols <- sample(1:50, 1)

n_elements <- sample(1:50, 1)

matrix1 <- matrix(sample(c(0, 1), n_rows*n_cols, replace = TRUE), ncol = n_cols)
matrix1

col_names <- c()
for (i in LETTERS) {
  for (j in LETTERS){
    col_names <- append(col_names, paste(i,j))
    if (length(col_names) == n_cols){
      break
    }
  }
  if (length(col_names) == n_cols){
    break
  }
}

colnames(matrix1) <- col_names[1:n_cols]
rownames(matrix1) <- 1:n_rows

dane <- matrix1
#dane <- m #dla przykładu ze sprawozdania
dane<-as.data.frame(dane)
View(dane)
dane_temp<-dane

wysokosc<-as.numeric(length(t(dane))/length(dane))
                      
############################################
numery_pokojow_z_kamera<-c()
nazwy_kamer<-c()

while (length(numery_pokojow_z_kamera) != wysokosc) {
  maks_kol_with_name<-which.max(colSums(dane_temp))
  maks_kol_num<-as.numeric(which.max(colSums(dane_temp)))
  
  for (i in c(1:wysokosc)) {
    
    if (dane_temp [i,maks_kol_num] == 1){
      numery_pokojow_z_kamera<-c(numery_pokojow_z_kamera,rownames(dane_temp[i,]))
      
      dane_temp[i,]<-0
      
      if(!(paste(names(maks_kol_with_name)) %in% nazwy_kamer)){
        nazwy_kamer<-c(nazwy_kamer,names(maks_kol_with_name))
      }
    }
  }
  
}

nazwy_kamer
numery_pokojow_z_kamera
