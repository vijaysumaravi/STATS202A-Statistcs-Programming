
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math

def accuracy(S,y):
    return np.mean((S*y))

def sigmoid(x):
    return 1 / (1 + np.exp(-x))
    
def train(X, Y, Xtest, Ytest, m=100, num_iterations=4000):
    n, p = X.shape
    p=p+1
    ntest,ptest = Xtest.shape
    th=0.4
    #X1=cbind(X>0.1,X>0.2,X>0.3)
    Y=2*Y-1
    Ytest=2*Ytest-1
    X1=X>th
    X1=2*X1-1
    Xtest1=Xtest>th
    Xtest1=2*Xtest1-1
    intr = np.ones(shape=(n,1))
    intr1 = np.ones(shape=(ntest,1)) 
    X1=np.concatenate((intr,X1),axis=1)
    Xtest1=np.concatenate((intr1,Xtest1),axis=1)
    accuracies_train = []
    accuracies_test = []
    beta = np.zeros(shape=(p,1))
    #beta = np.random.random_sample((p,1))
    for it in range(1,num_iterations+1):
        
        S=np.dot(X1,beta)
        W=np.exp(-Y*S)
        Y_train = np.sign(S)
        accuracies_train.append(accuracy(Y_train,Y))
        Stest=np.dot(Xtest1,beta)
        Y_test = np.sign(Stest)
        accuracies_test.append(accuracy(Y_test, Ytest))
        intr2 = np.ones(shape=(1,n))
        temp=np.zeros(shape=(n,p))
        temp=W*Y
        temp=temp*X1
        temp=temp/n
        a=np.dot(intr2,temp)
        e=(1-a)/2
        #print e.shape
        #j=min(e)
        j=e.argmin()
        #j=e.index(min(e.transpose()))
        #print j
        #print(beta.shape)
        z=0.5*math.log((1-e[:,j])/e[:,j])
        #print z
        beta[j,0]=beta[j,0]+z
      
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


def display_samples(digits, labels, n_samples=5):
    """
    Display random samples from the training set for each label.
    """
    distinct_label = np.unique(labels)

    fig_rows = len(distinct_label)
    fig_cols = n_samples
    fig_num = 1

    fig = plt.figure(1)
    fig.suptitle('Random samples of training data')
    for label in distinct_label:
        # random choose samples to display
        choice = np.random.choice(np.ix_(labels == label)[0], n_samples)
        for idx in choice:
            ax = fig.add_subplot(fig_rows, fig_cols, fig_num)
            fig.subplots_adjust(wspace=0, hspace=0)
            ax.set_title(labels[idx])
            ax.imshow(digits[idx].reshape(8,8), cmap=plt.cm.gray_r)
            ax.axis('off')
            fig_num += 1
    plt.show()


def split_samples(digits, labels):
    """Split the data into a training set (70%) and a testing set (30%)."""
    num_samples = digits.shape[0]
    #print num_samples
    num_training = int(round(num_samples * 0.7))
    #print num_training
    indices = np.random.permutation(num_samples)
    #print indices
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