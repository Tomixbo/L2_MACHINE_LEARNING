#Dataset
data1 = read.csv("C:/Users/ASUS_TMX/Documents/INSI_L2/Machine_Learning/CODE/LINEAR_REGRESSION/TP01_Linear_Regression/data/ex1data1.txt", col.names=c("x","y"),header=FALSE)
head(data1)

#set x, y, m
x=data1[,1]
y=data1[,2]
m=length(x)

# cost function
compute_cost <- function(w,b,x,y){
  total_cost = 0.0
  for (i in 1:m) 
  {
    f = (w*x[i]+b)
    total_cost = total_cost + ((f-y[i])^2)
    
  }
  total_cost=total_cost*(1/(2*m))
  total_cost
  return(total_cost)
}

# Compute cost with some initial values for paramaters w, b
initial_w = 2
initial_b = 1
compute_cost(initial_w, initial_b, x, y)

#gradient descent
compute_gradient <- function(w,b,x,y){
  dj_dw = 0
  dj_db = 0
  for (i in 1:m) 
  {
    f=w*x[i]+b
    dj_dw = dj_dw + (f-y[i])*x[i]
    dj_db = dj_db + (f-y[i])
  }
  dj_db = dj_db/m
  dj_dw = dj_dw/m
  return(c(dj_dw, dj_db))
}

# Compute and display cost and gradient with non-zero w
test_w = 0.2
test_b = 0.2
tmp_dj = compute_gradient(test_w, test_b, x, y)
print('Gradient at test w, b :')
print(tmp_dj)

