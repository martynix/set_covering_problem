#setwd('C:/Users/marty/Desktop/GITHUB/proj_optymal')
setwd('/Users/mateuszpindyk/Documents/GitHub/set_covering_problem')
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
