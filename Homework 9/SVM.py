
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def accuracy(S,y):
    return np.mean((S*y))

def sigmoid(x):
    return 1 / (1 + np.exp(-x))
    
def train(X, Y, Xtest, Ytest, m=100, num_iterations=4000, learning_rate=1e-1, lamda=0.01):
    n, p = X.shape
    p=p+1
    ntest,ptest = Xtest.shape
    Y=2*Y-1
    Ytest=2*Ytest-1
    intr = np.ones(shape=(n,1))
    intr1 = np.ones(shape=(ntest,1)) 
    X1=np.concatenate((intr,X),axis=1)
    Xtest1=np.concatenate((intr1,Xtest),axis=1)
    accuracies_train = []
    accuracies_test = []
    beta = np.zeros(shape=(p,1))
    #beta = np.random.random_sample((p,1))
    for it in range(1,num_iterations+1):
        S=np.dot(X1,beta)
        db=S*Y<1
        Y_train = np.sign(S)
        accuracies_train.append(accuracy(Y_train,Y))
        Stest=np.dot(Xtest1,beta)
        dbtest=Stest*Ytest<1
        Y_test = np.sign(Stest)
        accuracies_test.append(accuracy(Y_test, Ytest))
        temp=np.zeros(shape=(n,p))
        temp=db*Y
        temp = (temp*X1)/n
        dbeta=np.dot(intr.transpose(),temp)
        beta=beta+learning_rate*dbeta.transpose()
        beta[1:p]=beta[1:p]-lamda*dbeta.transpose()[1:p]
      
        if it % 100 == 0:
            print 'After iteration %d, Train Accuracy = %f,Test Accuracy = %f ' % (it, accuracies_train[-1],accuracies_test[-1])
    return beta,accuracies_train,accuracies_test
    
def load_digits(subset=None, normalize=True):
    # load digits.csv, adopted from sklearn.
    df = pd.read_csv('digits.csv')

    # only keep the numbers we want.
    if subset is not None:
        df = df[df.iloc[:,-1].isin(subset)]

    # convert to numpy arrays.
    digits = df.iloc[:,:-1].values.astype('float')
    labels = df.iloc[:,-1].values.astype('int')

    # Normalize digit values to 0 and 1.
    if normalize:
        digits -= digits.min()
        digits /= digits.max()

    # Change the labels to 0 and 1.
    for i in xrange(len(subset)):
        labels[labels == subset[i]] = i

    labels = labels.reshape((labels.shape[0], 1))
    return digits, labels



def split_samples(digits, labels):
    """Split the data into a training set (70%) and a testing set (30%)."""
    num_samples = digits.shape[0]
    num_training = int(round(num_samples * 0.7)) 
    indices = np.random.permutation(num_samples)
    training_idx, testing_idx = indices[:num_training], indices[num_training:]
    return (digits[training_idx], labels[training_idx],
            digits[testing_idx], labels[testing_idx])


digits, labels = load_digits(subset=[3, 5], normalize=True)

training_digits, training_labels, testing_digits, testing_labels = split_samples(digits, labels)
print '# training', training_digits.shape[0]
print '# testing', testing_digits.shape[0]
beta,accuracies_train,accuracies_test = train(training_digits, training_labels,testing_digits, testing_labels)
plt.figure(figsize=(10,5))
plt.subplot(1, 2, 1)
plt.ylabel('Training Accuracy')
plt.xlabel('Iteration')
plt.plot(range(len(accuracies_train)), accuracies_train)
plt.subplot(1, 2, 2)
plt.ylabel('Testing Accuracy')
plt.xlabel('Iteration')
plt.plot(range(len(accuracies_test)), accuracies_test)
plt.show()