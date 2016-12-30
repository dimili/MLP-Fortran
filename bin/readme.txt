TrainMBP.exe is used to train a neural network of MLP type. Input files are:
1.configuration.txt: It contains the parameter values of th neural network
2.networkstructure.txt: It contains the structure of the neural network and the size of the training set
3.traininput.txt: It contains the training set.

UseMBP.exe is used to calculate the output of a trained neural network which is fed with some input vector.
It uses the output.txt file which is created by TrainMBP.exe and contains the weights of the trained neural network.