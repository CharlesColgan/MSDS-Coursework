# -*- coding: utf-8 -*-
"""
Created on Sun Feb  7 19:46:38 2021

@author: charlescolgan
"""

#https://iq.opengenus.org/text-classification-using-k-nearest-neighbors/

#Libraries
import numpy as np
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.neighbors import KNeighborsClassifier
from sklearn import svm
from sklearn.pipeline import Pipeline
from datetime import datetime

start = datetime.now()

#Load Data

#Linguistic Data Consortium's New York Times Annotated Corpus

data_train = open("data_train.txt", "r")

data_valid = open("data_valid.txt", "r")

labels_train_original = open("labels_train_original.txt", "r")

labels_valid_original = open("labels_valid_original.txt", "r")

#Listize
dat_train = [line.strip() for line in data_train]

dat_val = [line.strip() for line in data_valid]

train_lab = [line.strip() for line in labels_train_original]

val_lab = [line.strip() for line in labels_valid_original]

#close files
data_train.close()

data_valid.close()

labels_train_original.close()

labels_valid_original.close()

#Endocde labels
for i in range(len(train_lab)):
    if train_lab[i] == "News":       
        train_lab[i] = 0        
    if train_lab[i] == "Opinion":
        train_lab[i] = 1        
    if train_lab[i] == "Classifieds":        
        train_lab[i] = 2        
    if train_lab[i] == "Features":        
        train_lab[i] = 3

for i in range(len(val_lab)):
    if val_lab[i] == "News":       
        val_lab[i] = 0        
    if val_lab[i] == "Opinion":
        val_lab[i] = 1        
    if val_lab[i] == "Classifieds":        
        val_lab[i] = 2        
    if val_lab[i] == "Features":        
        val_lab[i] = 3

#Form Algo

#Vectorize and featured by term frequency–inverse document frequency

#comparison of KNN and SVM for total accuracy over data

#KNN classifies by assuming a neighborhood around a data point containing
#K other data points and classifies by the average class of the contained data
def knn_text(train, labels, valid, valid_labels, n = 5):                       
    text_clf = Pipeline([
        ('vect', CountVectorizer()),
        ('tfidf', TfidfTransformer()),
        ('clf', KNeighborsClassifier(n_neighbors=n)),
    ])
    text_clf.fit(train, labels)
    predicted = text_clf.predict(valid)
    acc = np.mean(predicted == valid_labels)*100
    return [predicted, acc]

#SVM classifies by comparing two features and finding a seperator that
#splits the data into two classes 
def svm_text(train, labels, valid, valid_labels, n = 0.001):
    text_clf = Pipeline([
        ('vect', CountVectorizer()),
        ('tfidf', TfidfTransformer()),
        ('clf', svm.SVC(kernel = "linear", tol = n)),
    ])
    text_clf.fit(train, labels)
    predicted = text_clf.predict(valid)
    acc = np.mean(predicted == valid_labels)*100
    return [predicted, acc] 


#Process data       
N = list(range(1,26))

Res1 = []

Res2A = []

Res2B = []

Res2C = []

for i in N:
    Res1.append(knn_text(dat_train, train_lab, dat_val, val_lab, i)[1])
   
    Res2A.append(svm_text(dat_train, train_lab, dat_val, val_lab, i)[1])
    
    f = 1/i
    
    Res2B.append(svm_text(dat_train, train_lab, dat_val, val_lab, f)[1])
    
    g = np.linspace(1, 2, num = 25)[i-1]
    
    Res2C.append(svm_text(dat_train, train_lab, dat_val, val_lab, g)[1])
 
ResA = pd.DataFrame({"N":N, "KNN":Res1, "SVM":Res2A})
ResB = pd.DataFrame({"N":N, "KNN":Res1, "SVM":Res2B})
ResC = pd.DataFrame({"N":N, "KNN":Res1, "SVM":Res2C})

#Graph Results
ResA.plot(x = "N", y = ["KNN", "SVM"])

ResB.plot(x = "N", y = ["KNN", "SVM"])

ResC.plot(x = "N", y = ["KNN", "SVM"])

#SVM has better out put at all levels than KNN for the respectible scales of each. 
#However, the scaled parameters are differnet so this may be misleading.
#SVM likely worked better as a feature by feature comparison will likely find more distinct similarities,
#Where as KNN will see more data at once and be unable to distinguish

#Get process time
stop = datetime.now()

print()
print("Total time elapsed: ", stop - start)