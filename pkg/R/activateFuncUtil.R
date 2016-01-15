# Sigmoid function
sigmoid<-function(x) {
	y = 1/(1+exp(-x))
	return (y)
}

# Sigmoid Derivative Function
sigmoidDeri<-function(x){
	y = sigmoid(x)*(1-sigmoid(x))
	return (y)
}

tanhFunc<-function(x) {
	y = tanh(x)
	return (y)
}

tanhDeri<-function(x) {
	y = 1 - (tanh(x))^2
	return (y)
}

activate<-function(x,funName) {
	if (funName =='sigmoid') {
		res = sigmoid(x)
	} else if (funName == 'tanh') {
		res = tanhFunc(x)
	} else {
		res = "function name not found"
	}
	return (res)
}

activateDeri<-function(x,funName) {
	if (funName =='sigmoid') {
		res = sigmoidDeri(x)
	} else if (funName == 'tanh') {
		res = tanhDeri(x)
	} else {
		res = "function name not found"
	}
	return (res)
}
