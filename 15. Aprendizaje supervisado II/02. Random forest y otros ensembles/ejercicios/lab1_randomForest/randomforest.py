# -*- coding: utf-8 -*-
"""
Created on Sun Jan 31 15:47:28 2016

@author: Álvaro Barbero Jiménez
"""        

from cmath import sqrt
from sklearn.base import ClassifierMixin
from sklearn.tree import DecisionTreeClassifier

import numpy as np
import random
            
class RandomForest(ClassifierMixin):
    """Custom implementation of the Random Forest model for classification
    
    Inherits from scikit-learn ClassifierMixin, so we can make use of general model methods
    such as "score".
    """
    def __init__(self, nestimators):
        """Creates a new RandomForest
        
            nestimators: number of trees in the forest
        """
        self.nestimators = nestimators
        self._estimators = []
        
    def fit(self, X, y):
        """Fits the Random Forest to a given training dataset"""
        for i in range(self.nestimators):
            Xtrain, ytrain = bootstrap(X, y)
            decisiontree = DecisionTreeClassifier(max_features = 'sqrt')
            self._estimators.append(decisiontree.fit(Xtrain, ytrain))
        return self
    
    def predict(self, X):
        """Generates predictions for a given unlabeled dataset"""
        prediction_out = []
        predictions = [tree.predict(X) for tree in self._estimators]
        for i in range(len(predictions[0])):
            aux_list = [prediction[i] for prediction in predictions]
            prediction_out.append(most_common(aux_list))
        return np.array(prediction_out)

def bootstrap(X, y):
    """Returns a bootstrap sample of the given labeled dataset"""
    index = random.choices(range(len(X)), k = len(X))
    return (X[index], y[index])

def most_common(lst):
    return max(set(lst), key=lst.count)