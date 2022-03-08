# -*- coding: utf-8 -*-
"""
Created on Sun Jan 31 15:47:28 2016

@author: Álvaro Barbero Jiménez
"""        

from sklearn.base import ClassifierMixin
### START OF SOLUTION ###
import random
import numpy as np
from sklearn.tree import DecisionTreeClassifier
from scipy import stats
### END OF SOLUTION ###
            
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
        ### START OF SOLUTION ###
        for i in range(self.nestimators):
            Xb, yb = bootstrap(X, y)
            tree = DecisionTreeClassifier(max_features = "auto")
            self._estimators.append(tree.fit(Xb, yb))
        return self
        ### END OF SOLUTION ###
    
    def predict(self, X):
        """Generates predictions for a given unlabeled dataset"""
        ### START OF SOLUTION ###
        preds = np.array([tree.predict(X) for tree in self._estimators])
        return stats.mode(preds, axis=0)[0].squeeze()
        ### END OF SOLUTION ###


def bootstrap(X, y):
    """Returns a bootstrap sample of the given labeled dataset"""
    ### START OF SOLUTION ###
    idx = [random.randrange(len(X)) for i in range(len(X))]
    return X[idx], y[idx]
    ### END OF SOLUTION ###
